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
use middle::trans::common::*;
use middle::trans::datum::*;
use middle::trans::expr::{Dest, Ignore, SaveIn};
use middle::trans::expr;
use middle::trans::cleanup::CleanupMethods;
use middle::trans::build::{InsertElement,GEPi,VectorSplat,Store,FCmp,ICmp};
use middle::trans::build::{ExtractElement,And,ZExt};
use middle::ty;
use lib;
use lib::llvm::{ValueRef};
use middle::trans::type_::Type;

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
                    let count = ty::eval_repeat_count(&bcx.tcx(), count_expr);
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

    let mut res = C_i1(true);
    for i in range(0, simd_size) {
        let element = ExtractElement(bcx, simd_eq, C_i32(i as i32));
        res = And(bcx, res, element);
    }

    ZExt(bcx, res, Type::i8())
}

