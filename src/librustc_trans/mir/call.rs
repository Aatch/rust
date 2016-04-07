
use rustc::mir::repr as mir;
use rustc::ty;

use callee::Callee;
use common::{self, BlockAndBuilder};

use super::MirContext;

impl<'bcx, 'tcx> MirContext<'bcx, 'tcx> {

    pub fn trans_call(&mut self, bcx: &BlockAndBuilder<'bcx, 'tcx>,
                      func: &mir::Operand<'tcx>, args: &[mir::Operand<'tcx>],
                      destination: Option<&(mir::Lvalue<'tcx>, mir::BasicBlock)>,
                      cleanup: Option<BasicBlock>) {
        // Create the callee. This is a fn ptr or zero-sized and hence a kind of scalar.
        let callee = self.trans_operand(&bcx, func);

        let (mut callee, abi, sig) = match callee.ty.sty {
            ty::TyFnDef(def_id, substs, f) => {
                (Callee::def(bcx.ccx(), def_id, substs), f.abi, &f.sig)
            }
            ty::TyFnPtr(f) => {
                (Callee {
                    data: Fn(callee.immediate()),
                    ty: callee.ty
                }, f.abi, &f.sig)
            }
            _ => bug!("{} is not callable", callee.ty)
        };

        // Handle intrinsics old trans wants Expr's for, ourselves.
        let intrinsic = match (&callee.ty.sty, &callee.data) {
            (&ty::TyFnDef(def_id, _, _), &Intrinsic) => {
                Some(bcx.tcx().item_name(def_id).as_str())
            }
            _ => None
        };
        let intrinsic = intrinsic.as_ref().map(|s| &s[..]);

        if intrinsic == Some("move_val_init") {
            let &(_, target) = destination.as_ref().unwrap();
            // The first argument is a thin destination pointer.
            let llptr = self.trans_operand(&bcx, &args[0]).immediate();
            let val = self.trans_operand(&bcx, &args[1]);
            self.store_operand(&bcx, llptr, val);
            self.set_operand_dropped(&bcx, &args[1]);
            funclet_br(bcx, self.llblock(target));
            return;
        }

        if intrinsic == Some("transmute") {
            let &(ref dest, target) = destination.as_ref().unwrap();
            self.with_lvalue_ref(&bcx, dest, |this, dest| {
                this.trans_transmute(&bcx, &args[0], dest);
            });

            self.set_operand_dropped(&bcx, &args[0]);
            funclet_br(bcx, self.llblock(target));
            return;
        }

        let extra_args = &args[sig.0.inputs.len()..];
        let extra_args = extra_args.iter().map(|op_arg| {
            self.mir.operand_ty(bcx.tcx(), op_arg)
        }).collect::<Vec<_>>();
        let fn_ty = callee.direct_fn_type(bcx.ccx(), &extra_args);

        // The arguments we'll be passing. Plus one to account for outptr, if used.
        let arg_count = fn_ty.args.len() + fn_ty.ret.is_indirect() as usize;
        let mut llargs = Vec::with_capacity(arg_count);

        // Prepare the return value destination
        let ret_dest = if let Some((ref dest, _)) = *destination {
            let is_intrinsic = if let Intrinsic = callee.data {
                true
            } else {
                false
            };
            self.make_return_dest(&bcx, dest, &fn_ty.ret, &mut llargs, is_intrinsic)
        } else {
            ReturnDest::Nothing
        };

        // Split the rust-call tupled arguments off.
        let (first_args, untuple) = if abi == Abi::RustCall && !args.is_empty() {
            let (tup, args) = args.split_last().unwrap();
            (args, Some(tup))
        } else {
            (&args[..], None)
        };

        let mut idx = 0;
        for arg in first_args {
            let val = self.trans_operand(&bcx, arg).val;
            self.trans_argument(&bcx, val, &mut llargs, &fn_ty,
                                &mut idx, &mut callee.data);
        }
        if let Some(tup) = untuple {
            self.trans_arguments_untupled(&bcx, tup, &mut llargs, &fn_ty,
                                          &mut idx, &mut callee.data)
        }

        let fn_ptr = match callee.data {
            NamedTupleConstructor(_) => {
                // FIXME translate this like mir::Rvalue::Aggregate.
                callee.reify(bcx.ccx()).val
            }
            Intrinsic => {
                use callee::ArgVals;
                use expr::{Ignore, SaveIn};
                use intrinsic::trans_intrinsic_call;

                let (dest, llargs) = match ret_dest {
                    _ if fn_ty.ret.is_indirect() => {
                        (SaveIn(llargs[0]), &llargs[1..])
                    }
                    ReturnDest::Nothing => (Ignore, &llargs[..]),
                    ReturnDest::IndirectOperand(dst, _) |
                    ReturnDest::Store(dst) => (SaveIn(dst), &llargs[..]),
                    ReturnDest::DirectOperand(_) =>
                        bug!("Cannot use direct operand with an intrinsic call")
                };

                bcx.with_block(|bcx| {
                    let res = trans_intrinsic_call(bcx, callee.ty, &fn_ty,
                                                   ArgVals(llargs), dest,
                                                   DebugLoc::None);
                    let bcx = res.bcx.build();
                    if let Some((_, target)) = *destination {
                        for op in args {
                            self.set_operand_dropped(&bcx, op);
                        }
                        funclet_br(bcx, self.llblock(target));
                    } else {
                        // trans_intrinsic_call already used Unreachable.
                        // bcx.unreachable();
                    }
                });

                if let ReturnDest::IndirectOperand(dst, _) = ret_dest {
                    // Make a fake operand for store_return
                    let op = OperandRef {
                        val: OperandValue::Ref(dst),
                        ty: sig.0.output.unwrap()
                    };
                    self.store_return(&bcx, ret_dest, fn_ty.ret, op);
                }

                return;
            }
            Fn(f) => f,
            Virtual(_) => bug!("Virtual fn ptr not extracted")
        };

        // Many different ways to call a function handled here
        if let Some(cleanup) = cleanup.map(|bb| self.bcx(bb)) {
            let ret_bcx = if let Some((_, target)) = *destination {
                self.blocks[target.index()]
            } else {
                self.unreachable_block()
            };
            let landingpad = self.make_landing_pad(cleanup);

            let invokeret = bcx.invoke(fn_ptr,
                                       &llargs,
                                       ret_bcx.llbb,
                                       landingpad.llbb(),
                                       cleanup_bundle.as_ref());
            fn_ty.apply_attrs_callsite(invokeret);

            landingpad.at_start(|bcx| for op in args {
                self.set_operand_dropped(bcx, op);
            });

            if destination.is_some() {
                let ret_bcx = ret_bcx.build();
                ret_bcx.at_start(|ret_bcx| {
                    let op = OperandRef {
                        val: OperandValue::Immediate(invokeret),
                        ty: sig.0.output.unwrap()
                    };
                    self.store_return(&ret_bcx, ret_dest, fn_ty.ret, op);
                    for op in args {
                        self.set_operand_dropped(&ret_bcx, op);
                    }
                });
            }
        } else {
            let llret = bcx.call(fn_ptr, &llargs, cleanup_bundle.as_ref());
            fn_ty.apply_attrs_callsite(llret);
            if let Some((_, target)) = *destination {
                let op = OperandRef {
                    val: OperandValue::Immediate(llret),
                    ty: sig.0.output.unwrap()
                };
                self.store_return(&bcx, ret_dest, fn_ty.ret, op);
                for op in args {
                    self.set_operand_dropped(&bcx, op);
                }
                funclet_br(bcx, self.llblock(target));
            } else {
                // no need to drop args, because the call never returns
                bcx.unreachable();
            }
        }

    }
}
