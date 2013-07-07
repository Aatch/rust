// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


use back::link::mangle_exported_name;
use driver::session;
use lib::llvm::ValueRef;
use middle::trans::base::{set_inline_hint_if_appr, set_inline_hint};
use middle::trans::base::{trans_enum_variant,push_ctxt};
use middle::trans::base::{trans_fn, decl_internal_cdecl_fn};
use middle::trans::base::{get_item_val, no_self};
use middle::trans::base;
use middle::trans::common::*;
use middle::trans::datum;
use middle::trans::foreign;
use middle::trans::machine;
use middle::trans::meth;
use middle::trans::type_of::type_of_fn_from_ty;
use middle::trans::type_of;
use middle::trans::type_use;
use middle::ty;
use middle::ty::{FnSig};
use middle::typeck;
use util::ppaux::{Repr,ty_to_str};

use syntax::ast;
use syntax::ast_map;
use syntax::ast_map::path_name;
use syntax::ast_util::local_def;
use syntax::opt_vec;
use syntax::abi::AbiSet;

pub fn monomorphic_fn(ccx: @mut CrateContext,
                      fn_id: ast::def_id,
                      real_substs: &ty::substs,
                      vtables: Option<typeck::vtable_res>,
                      self_vtable: Option<typeck::vtable_origin>,
                      impl_did_opt: Option<ast::def_id>,
                      ref_id: Option<ast::node_id>)
    -> (ValueRef, bool)
{
    debug!("monomorphic_fn(\
            fn_id=%s, \
            real_substs=%s, \
            vtables=%s, \
            self_vtable=%s, \
            impl_did_opt=%s, \
            ref_id=%?)",
           fn_id.repr(ccx.tcx),
           real_substs.repr(ccx.tcx),
           vtables.repr(ccx.tcx),
           self_vtable.repr(ccx.tcx),
           impl_did_opt.repr(ccx.tcx),
           ref_id);

    assert!(real_substs.tps.iter().all(|t| !ty::type_needs_infer(*t)));
    let _icx = push_ctxt("monomorphic_fn");
    let mut must_cast = false;
    let substs = real_substs.tps.iter().transform(|t| {
        match normalize_for_monomorphization(ccx.tcx, *t) {
          Some(t) => { must_cast = true; t }
          None => *t
        }
    }).collect::<~[ty::t]>();

    for real_substs.tps.iter().advance |s| { assert!(!ty::type_has_params(*s)); }
    for substs.iter().advance |s| { assert!(!ty::type_has_params(*s)); }
    let param_uses = type_use::type_uses_for(ccx, fn_id, substs.len());

    let psubsts = @param_substs {
        tys: substs,
        vtables: vtables,
        self_ty: real_substs.self_ty,
        self_vtable: self_vtable
    };

    let hash_id = make_mono_id(ccx, fn_id, impl_did_opt,
                               &*psubsts,
                               Some(param_uses));
    if hash_id.params.iter().any(
                |p| match *p { mono_precise(_, _) => false, _ => true }) {
        must_cast = true;
    }

    debug!("monomorphic_fn(\
            fn_id=%s, \
            psubsts=%s, \
            hash_id=%?)",
           fn_id.repr(ccx.tcx),
           psubsts.repr(ccx.tcx),
           hash_id);

    match ccx.monomorphized.find(&hash_id) {
      Some(&val) => {
        debug!("leaving monomorphic fn %s",
               ty::item_path_str(ccx.tcx, fn_id));
        return (val, must_cast);
      }
      None => ()
    }

    let tpt = ty::lookup_item_type(ccx.tcx, fn_id);
    let llitem_ty = tpt.ty;

    let map_node = session::expect(
        ccx.sess,
        ccx.tcx.items.find_copy(&fn_id.node),
        || fmt!("While monomorphizing %?, couldn't find it in the item map \
                 (may have attempted to monomorphize an item \
                 defined in a different crate?)", fn_id));
    // Get the path so that we can create a symbol
    let (pt, name, span) = match map_node {
      ast_map::NodeItem(i, ref pt) => (pt, i.ident, i.span),
      ast_map::NodeVariant(v, enm, ref pt) => (pt, v.node.name, enm.span),
      ast_map::NodeMethod(m, _, ref pt) => (pt, m.ident, m.span),
      ast_map::NodeForeignItem(i, abis, _, ref pt) if abis.is_intrinsic()
      => (pt, i.ident, i.span),
      ast_map::NodeForeignItem(*) => {
        // Foreign externs don't have to be monomorphized.
        return (get_item_val(ccx, fn_id.node), true);
      }
      ast_map::NodeTraitMethod(&ast::provided(m), _, ref pt) => {
        (pt, m.ident, m.span)
      }
      ast_map::NodeTraitMethod(&ast::required(_), _, _) => {
        ccx.tcx.sess.bug("Can't monomorphize a required trait method")
      }
      ast_map::NodeExpr(*) => {
        ccx.tcx.sess.bug("Can't monomorphize an expr")
      }
      ast_map::NodeStmt(*) => {
        ccx.tcx.sess.bug("Can't monomorphize a stmt")
      }
      ast_map::NodeArg(*) => ccx.tcx.sess.bug("Can't monomorphize an arg"),
      ast_map::NodeBlock(*) => {
          ccx.tcx.sess.bug("Can't monomorphize a block")
      }
      ast_map::NodeLocal(*) => {
          ccx.tcx.sess.bug("Can't monomorphize a local")
      }
      ast_map::NodeCalleeScope(*) => {
          ccx.tcx.sess.bug("Can't monomorphize a callee-scope")
      }
      ast_map::NodeStructCtor(_, i, ref pt) => (pt, i.ident, i.span)
    };

    let mono_ty = ty::subst_tps(ccx.tcx, psubsts.tys,
                                psubsts.self_ty, llitem_ty);
    let llfty = type_of_fn_from_ty(ccx, mono_ty);

    ccx.stats.n_monos += 1;

    let depth = match ccx.monomorphizing.find(&fn_id) {
        Some(&d) => d, None => 0
    };
    // Random cut-off -- code that needs to instantiate the same function
    // recursively more than thirty times can probably safely be assumed to be
    // causing an infinite expansion.
    if depth > 30 {
        ccx.sess.span_fatal(
            span, "overly deep expansion of inlined function");
    }
    ccx.monomorphizing.insert(fn_id, depth + 1);

    let elt = path_name(gensym_name(ccx.sess.str_of(name)));
    let mut pt = /* bad */copy (*pt);
    pt.push(elt);
    let s = mangle_exported_name(ccx, pt, mono_ty);
    debug!("monomorphize_fn mangled to %s", s);

    let mk_lldecl = || {
        let lldecl = decl_internal_cdecl_fn(ccx.llmod, /*bad*/copy s, llfty);
        ccx.monomorphized.insert(hash_id, lldecl);
        lldecl
    };

    let lldecl = match map_node {
      ast_map::NodeItem(i @ ast::item {
                node: ast::item_fn(ref decl, _, _, _, ref body),
                _
            }, _) => {
        let d = mk_lldecl();
        set_inline_hint_if_appr(/*bad*/copy i.attrs, d);
        trans_fn(ccx,
                 pt,
                 decl,
                 body,
                 d,
                 no_self,
                 Some(psubsts),
                 fn_id.node,
                 []);
        d
      }
      ast_map::NodeItem(*) => {
          ccx.tcx.sess.bug("Can't monomorphize this kind of item")
      }
      ast_map::NodeForeignItem(i, _, _, _) => {
          let d = mk_lldecl();
          foreign::trans_intrinsic(ccx, d, i, pt, psubsts, i.attrs,
                                ref_id);
          d
      }
      ast_map::NodeVariant(ref v, enum_item, _) => {
        let tvs = ty::enum_variants(ccx.tcx, local_def(enum_item.id));
        let this_tv = *tvs.iter().find_(|tv| { tv.id.node == fn_id.node}).get();
        let d = mk_lldecl();
        set_inline_hint(d);
        match v.node.kind {
            ast::tuple_variant_kind(ref args) => {
                trans_enum_variant(ccx, enum_item.id, v, /*bad*/copy *args,
                                   this_tv.disr_val, Some(psubsts), d);
            }
            ast::struct_variant_kind(_) =>
                ccx.tcx.sess.bug("can't monomorphize struct variants"),
        }
        d
      }
      ast_map::NodeMethod(mth, _, _) => {
        // XXX: What should the self type be here?
        let d = mk_lldecl();
        set_inline_hint_if_appr(/*bad*/copy mth.attrs, d);
        meth::trans_method(ccx, pt, mth, Some(psubsts), d);
        d
      }
      ast_map::NodeTraitMethod(@ast::provided(mth), _, pt) => {
        let d = mk_lldecl();
        set_inline_hint_if_appr(/*bad*/copy mth.attrs, d);
        meth::trans_method(ccx, /*bad*/copy *pt, mth, Some(psubsts), d);
        d
      }
      ast_map::NodeStructCtor(struct_def, _, _) => {
        let d = mk_lldecl();
        set_inline_hint(d);
        base::trans_tuple_struct(ccx,
                                 /*bad*/copy struct_def.fields,
                                 struct_def.ctor_id.expect("ast-mapped tuple struct \
                                                            didn't have a ctor id"),
                                 Some(psubsts),
                                 d);
        d
      }

      // Ugh -- but this ensures any new variants won't be forgotten
      ast_map::NodeExpr(*) |
      ast_map::NodeStmt(*) |
      ast_map::NodeTraitMethod(*) |
      ast_map::NodeArg(*) |
      ast_map::NodeBlock(*) |
      ast_map::NodeCalleeScope(*) |
      ast_map::NodeLocal(*) => {
        ccx.tcx.sess.bug(fmt!("Can't monomorphize a %?", map_node))
      }
    };
    ccx.monomorphizing.insert(fn_id, depth);

    debug!("leaving monomorphic fn %s", ty::item_path_str(ccx.tcx, fn_id));
    (lldecl, must_cast)
}

pub fn normalize_for_monomorphization(tcx: ty::ctxt,
                                      ty: ty::t) -> Option<ty::t> {
    // FIXME[mono] could do this recursively. is that worthwhile? (#2529)
    return match ty::get(ty).sty {
        ty::ty_box(*) => {
            Some(ty::mk_opaque_box(tcx))
        }
        ty::ty_bare_fn(_) => {
            Some(ty::mk_bare_fn(
                tcx,
                ty::BareFnTy {
                    purity: ast::impure_fn,
                    abis: AbiSet::Rust(),
                    sig: FnSig {bound_lifetime_names: opt_vec::Empty,
                                inputs: ~[],
                                output: ty::mk_nil()}}))
        }
        ty::ty_closure(ref fty) => {
            Some(normalized_closure_ty(tcx, fty.sigil))
        }
        ty::ty_trait(_, _, ref store, _, _) => {
            let sigil = match *store {
                ty::UniqTraitStore => ast::OwnedSigil,
                ty::BoxTraitStore => ast::ManagedSigil,
                ty::RegionTraitStore(_) => ast::BorrowedSigil,
            };

            // Traits have the same runtime representation as closures.
            Some(normalized_closure_ty(tcx, sigil))
        }
        ty::ty_ptr(_) => {
            Some(ty::mk_uint())
        }
        _ => {
            None
        }
    };

    fn normalized_closure_ty(tcx: ty::ctxt,
                             sigil: ast::Sigil) -> ty::t
    {
        ty::mk_closure(
            tcx,
            ty::ClosureTy {
                purity: ast::impure_fn,
                sigil: sigil,
                onceness: ast::Many,
                region: ty::re_static,
                bounds: ty::EmptyBuiltinBounds(),
                sig: ty::FnSig {bound_lifetime_names: opt_vec::Empty,
                                inputs: ~[],
                                output: ty::mk_nil()}})
    }
}

pub fn make_mono_id(ccx: @mut CrateContext,
                    item: ast::def_id,
                    impl_did_opt: Option<ast::def_id>,
                    substs: &param_substs,
                    param_uses: Option<@~[type_use::type_uses]>) -> mono_id {
    // FIXME (possibly #5801): Need a lot of type hints to get
    // .collect() to work.
    let substs_iter = substs.self_ty.iter().chain_(substs.tys.iter());
    let precise_param_ids: ~[(ty::t, Option<@~[mono_id]>)] = match substs.vtables {
      Some(vts) => {
        debug!("make_mono_id vtables=%s substs=%s",
               vts.repr(ccx.tcx), substs.tys.repr(ccx.tcx));
        let self_vtables = substs.self_vtable.map(|vtbl| @~[copy *vtbl]);
        let vts_iter = self_vtables.iter().chain_(vts.iter());
        vts_iter.zip(substs_iter).transform(|(vtable, subst)| {
            let v = vtable.map(|vt| meth::vtable_id(ccx, vt));
            (*subst, if !v.is_empty() { Some(@v) } else { None })
        }).collect()
      }
      None => substs_iter.transform(|subst| (*subst, None::<@~[mono_id]>)).collect()
    };


    let param_ids = match param_uses {
      Some(ref uses) => {
        // param_uses doesn't include a use for the self type.
        // We just say it is fully used.
        let self_use =
            substs.self_ty.map(|_| type_use::use_repr|type_use::use_tydesc);
        let uses_iter = self_use.iter().chain_(uses.iter());

        precise_param_ids.iter().zip(uses_iter).transform(|(id, uses)| {
            if ccx.sess.no_monomorphic_collapse() {
                match copy *id {
                    (a, b) => mono_precise(a, b)
                }
            } else {
                match *id {
                    (a, b@Some(_)) => mono_precise(a, b),
                    (subst, None) => {
                        if *uses == 0 {
                            mono_any
                        } else if *uses == type_use::use_repr &&
                            !ty::type_needs_drop(ccx.tcx, subst)
                        {
                            let llty = type_of::type_of(ccx, subst);
                            let size = machine::llbitsize_of_real(ccx, llty);
                            let align = machine::llalign_of_min(ccx, llty);
                            let mode = datum::appropriate_mode(ccx.tcx, subst);
                            let data_class = mono_data_classify(subst);

                            debug!("make_mono_id: type %s -> size %u align %u mode %? class %?",
                                  ty_to_str(ccx.tcx, subst),
                                  size, align, mode, data_class);

                            // Special value for nil to prevent problems
                            // with undef return pointers.
                            if size <= 8u && ty::type_is_nil(subst) {
                                mono_repr(0u, 0u, data_class, mode)
                            } else {
                                mono_repr(size, align, data_class, mode)
                            }
                        } else {
                            mono_precise(subst, None)
                        }
                    }
                }
            }
        }).collect()
      }
      None => {
          precise_param_ids.iter().transform(|x| {
              let (a, b) = copy *x;
              mono_precise(a, b)
          }).collect()
      }
    };
    @mono_id_ {def: item, params: param_ids, impl_did_opt: impl_did_opt}
}
