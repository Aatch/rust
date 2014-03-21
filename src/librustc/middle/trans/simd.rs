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
use middle::trans::build::{InsertElement,GEPi,VectorSplat,Store};
use middle::ty;

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
