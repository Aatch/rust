// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use syntax::ast;
use syntax::parse::token;
use middle::trans::common::*;
use middle::trans::datum::*;
use middle::trans::expr::{Dest, Ignore, SaveIn};
use middle::trans::expr;
use middle::trans::cleanup::CleanupMethods;
use middle::trans::build::{InsertElement,GEPi,VectorSplat,Store,FCmp,ICmp};
use middle::trans::build::{ExtractElement,And,ZExt,ShuffleVector,Load};
use middle::ty;
use middle::trans::type_of;
use lib;
use lib::llvm::{ValueRef,llvm};
use middle::trans::type_::Type;

use std::vec_ng::Vec;

pub fn trans_simd<'a>(bcx: &'a Block<'a>,
                      expr: &ast::Expr,
                      dest: expr::Dest) -> &'a Block<'a> {

    let fcx = bcx.fcx;
    let mut bcx = bcx;

    debug!("trans_simd(expr={}, dest={})",
            bcx.expr_to_str(expr), dest.to_str(bcx.ccx()));

    match expr.node {
        ast::ExprSimd(ref elems) => {
            match dest {
                Ignore => {
                    for el in elems.iter() {
                        bcx = expr::trans_into(bcx, *el, Ignore);
                    }
                }
                SaveIn(mut lldest) => {
                    for (i, elem) in elems.iter().enumerate() {
                        let lleltptr = GEPi(bcx, lldest, [0, i]);
                        bcx = expr::trans_into(bcx, *elem, SaveIn(lleltptr));
                    }
                }
            }
            return bcx;
        }
        ast::ExprSimdRepeat(elem, count_expr) => {
            match dest {
                Ignore => {
                    return expr::trans_into(bcx, elem, Ignore);
                }
                SaveIn(lldest) => {
                    let count = ty::eval_repeat_count(bcx.tcx(), count_expr);
                    if count == 0 {
                        bcx.tcx().sess.span_bug(count_expr.span, "Unexpected zero count");
                    }

                    let elem = unpack_datum!(bcx, expr::trans(bcx, elem));
                    assert!(!ty::type_moves_by_default(bcx.tcx(), elem.ty));

                    let vec = VectorSplat(bcx, count, elem.to_llscalarish(bcx));
                    Store(bcx, vec, lldest);
                    return bcx;
                }
            }
        }
        _ => bcx.tcx().sess.span_bug(expr.span, "Unexpected non-SIMD expression in trans_simd")
    }
}

pub fn trans_simd_eqop<'a>(bcx: &'a Block<'a>,
                       lhs: ValueRef,
                       rhs: ValueRef,
                       simdty: ty::t) -> ValueRef {

    let tcx = bcx.tcx();
    let sub_ty = ty::simd_type(tcx, simdty);
    let simd_size = ty::simd_size(tcx, simdty);

    let is_float = ty::type_is_fp(sub_ty);

    let simd_eq = if is_float {
        FCmp(bcx, lib::llvm::RealOEQ, lhs, rhs)
    } else {
        ICmp(bcx, lib::llvm::IntEQ, lhs, rhs)
    };

    let mut res = C_i1(bcx.ccx(), true);
    for i in range(0, simd_size) {
        let element = ExtractElement(bcx, simd_eq, C_i32(bcx.ccx(), i as i32));
        res = And(bcx, res, element);
    }

    ZExt(bcx, res, Type::i8(bcx.ccx()))
}

pub fn trans_shuffle<'a>(bcx: &'a Block<'a>,
                         base: Datum<Lvalue>,
                         field: ast::Ident) -> DatumBlock<'a, Expr> {
    let ccx = bcx.ccx();

    let field = token::get_ident(field);
    let name = field.get();
    let (len, mask) = field_name_to_mask(ccx, name);

    let sub_ty = ty::simd_type(bcx.tcx(), base.ty);
    let elt_ty = type_of::type_of(bcx.ccx(), sub_ty);
    let num_elts = ty::simd_size(bcx.tcx(), base.ty);

    let vec = Load(bcx, base.to_llref());
    let vec2 = unsafe {
        llvm::LLVMGetUndef(Type::vector(&elt_ty, num_elts as u64).to_ref())
    };

    debug!("trans_shuffle [{}, {}, {}]",
        ccx.tn.val_to_str(vec), ccx.tn.val_to_str(vec2), ccx.tn.val_to_str(mask));

    let shuffled = ShuffleVector(bcx, vec, vec2, mask);

    let datum = Datum(shuffled, ty::mk_simd(bcx.tcx(), sub_ty, len),
        RvalueExpr(Rvalue(ByValue)));

    DatumBlock { datum: datum, bcx: bcx }
}

fn field_name_to_mask(ccx: &CrateContext, mut name: &str) -> (uint, ValueRef) {

    if name[0] == 0x73 { // if it begins with a 's'
        name = name.slice_from(1);
    }

    let vec : Vec<ValueRef> = name.chars().map(|c| {
        match c {
            '0'..'9' => {
                let val = (c as uint) - ('0' as uint);
                C_i32(ccx, val as i32)
            }
            'a'..'f' => {
                let val = (c as uint) - ('a' as uint);
                C_i32(ccx, val as i32)
            }
            'x' => C_i32(ccx, 0),
            'y' => C_i32(ccx, 1),
            'z' => C_i32(ccx, 2),
            'w' => C_i32(ccx, 3),
            _ => fail!("Invalid position in shuffle access")
        }
    }).collect();

    (vec.len(), C_vector(vec.as_slice()))
}
