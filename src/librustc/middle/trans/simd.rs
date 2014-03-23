// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std;

use syntax::ast;
use syntax::parse::token;
use middle::trans::base;
use middle::trans::build::{GEPi,VectorSplat,Store,FCmp,ICmp};
use middle::trans::build::{ExtractElement,And,ZExt,ShuffleVector,Load,InBoundsGEP};
use middle::trans::common::*;
use middle::trans::cleanup::CleanupMethods;
use middle::trans::datum::*;
use middle::trans::expr::{Dest, Ignore, SaveIn, trans_to_lvalue, trans};
use middle::trans::expr;
use middle::ty;
use middle::trans::type_of;
use lib;
use lib::llvm::{ValueRef,TypeRef,llvm};
use middle::trans::type_::Type;

pub fn trans_simd<'a>(bcx: &'a Block<'a>,
                      expr: &ast::Expr,
                      dest: expr::Dest) -> &'a Block<'a> {

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
                SaveIn(lldest) => {
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
                        bcx.tcx().sess.span_bug(count_expr.span, "unexpected zero count");
                    }

                    let elem = unpack_datum!(bcx, expr::trans(bcx, elem));
                    assert!(!ty::type_moves_by_default(bcx.tcx(), elem.ty));

                    let vec = VectorSplat(bcx, count, elem.to_llscalarish(bcx));
                    Store(bcx, vec, lldest);
                    return bcx;
                }
            }
        }
        _ => bcx.tcx().sess.span_bug(expr.span, "unexpected non-SIMD expression in trans_simd")
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
    let datum = if len == 1 {
        base.get_element(sub_ty, |src| {
            InBoundsGEP(bcx, src, [C_i32(ccx, 0), mask])
        }).to_expr_datum()
    } else {
        let elt_ty = type_of::type_of(bcx.ccx(), sub_ty);
        let num_elts = ty::simd_size(bcx.tcx(), base.ty);

        let vec = Load(bcx, base.to_llref());
        let vec2 = unsafe {
            llvm::LLVMGetUndef(Type::vector(&elt_ty, num_elts as u64).to_ref())
        };

        debug!("trans_shuffle [{}, {}, {}]",
            ccx.tn.val_to_str(vec), ccx.tn.val_to_str(vec2), ccx.tn.val_to_str(mask));

        let shuffled = ShuffleVector(bcx, vec, vec2, mask);

        Datum(shuffled, ty::mk_simd(bcx.tcx(), sub_ty, len),
            RvalueExpr(Rvalue(ByValue)))
    };

    DatumBlock { datum: datum, bcx: bcx }
}

pub fn trans_shuffle_assign<'a>(bcx: &'a Block<'a>,
                                dst: &ast::Expr,
                                base: &ast::Expr,
                                field: ast::Ident,
                                src: &ast::Expr) -> &'a Block<'a> {
    let mut bcx = bcx;
    let ccx = bcx.ccx();
    let _icx = base::push_ctxt("trans_shuffle_assign");

    let base_ty = expr_ty_adjusted(bcx, base);
    let result_size = ty::simd_size(bcx.tcx(), base_ty);

    // Check to see if the src expression is a shuffle as well
    match src.node {
        ast::ExprField(src_base, src_field, _) => {
            let src_base_ty = expr_ty_adjusted(bcx, src_base);
            // Ok, so both the dst and the src are simd shuffles, we can
            // do this using a single shufflevector instruction
            if ty::type_is_simd(bcx.tcx(), src_base_ty) {
                // Calculate the mask for the shuffle by starting with the identity
                // mask and then replacing components based on the pair on the LHS and the
                // RHS
                let base_field = token::get_ident(field);
                let mut base_name = base_field.get();
                let src_field = token::get_ident(src_field);
                let mut src_name = src_field.get();

                if base_name[0] == 's' as u8 {
                    base_name = base_name.slice_from(1);
                }

                if src_name[0] == 's' as u8 {
                    src_name = src_name.slice_from(1);
                }

                let vec_size = std::cmp::max(
                    result_size,
                    ty::simd_size(bcx.tcx(), src_base_ty));

                let mut mask: Vec<uint> = range(0, result_size).collect();
                for (lhs, rhs) in base_name.chars().zip(src_name.chars()) {
                    let lhs_component = char_to_component(lhs);
                    let rhs_component = char_to_component(rhs);

                    *mask.get_mut(lhs_component) = rhs_component + vec_size;
                }

                let mask = C_vector(mask.map(|&c| C_i32(bcx.ccx(), c as i32)).as_slice());

                bcx.fcx.push_ast_cleanup_scope(src.id);
                let src_datum = unpack_datum!(bcx, trans(bcx, src_base));
                bcx = bcx.fcx.pop_and_trans_ast_cleanup_scope(bcx, src.id);

                bcx.fcx.push_ast_cleanup_scope(dst.id);
                let base_datum = unpack_datum!(bcx, trans_to_lvalue(bcx, base, "shuffle_base"));
                bcx = bcx.fcx.pop_and_trans_ast_cleanup_scope(bcx, dst.id);

                let base_vec = base_datum.to_llscalarish(bcx);
                let src_vec = src_datum.to_llscalarish(bcx);

                debug!("trans_shuffle_assign: base_vec={}, src_vec={}, mask={}",
                    ccx.tn.val_to_str(base_vec), ccx.tn.val_to_str(src_vec),
                    ccx.tn.val_to_str(mask));

                let result = shuffle_vector(bcx, base_vec, src_vec, mask);

                let base_dest = base_datum.to_llref();
                Store(bcx, result, base_dest);

                return bcx;
            }
        }
        _ => {}
    }

    bcx.fcx.push_ast_cleanup_scope(dst.id);
    let base_datum = unpack_datum!(bcx, trans_to_lvalue(bcx, base, "shuffle_base"));
    bcx = bcx.fcx.pop_and_trans_ast_cleanup_scope(bcx, dst.id);

    let base_field = token::get_ident(field);
    let mut base_name = base_field.get();

    if base_name[0] == 's' as u8 {
        base_name = base_name.slice_from(1);
    }

    let src_datum = unpack_datum!(bcx, trans(bcx, src));

    let vec_size = std::cmp::max(
        result_size,
        ty::simd_size(bcx.tcx(), src_datum.ty));

    let mut mask : Vec<uint> = range(0, result_size).collect();
    for (i, c) in base_name.chars().enumerate() {
        let component = char_to_component(c);

        *mask.get_mut(component) = i + vec_size;
    }

    let mask = C_vector(mask.map(|&c| C_i32(bcx.ccx(), c as i32)).as_slice());

    let base_vec = base_datum.to_llscalarish(bcx);
    let src_vec = src_datum.to_llscalarish(bcx);

    debug!("trans_shuffle_assign: base_vec={}, src_vec={}, mask={}",
        ccx.tn.val_to_str(base_vec), ccx.tn.val_to_str(src_vec),
        ccx.tn.val_to_str(mask));

    let result = shuffle_vector(bcx, base_vec, src_vec, mask);

    let base_dest = base_datum.to_llref();
    Store(bcx, result, base_dest);

    return bcx;
}

fn field_name_to_mask(ccx: &CrateContext, mut name: &str) -> (uint, ValueRef) {

    if name[0] == 's' as u8 { // if it begins with a 's'
        name = name.slice_from(1);
    }

    let vec : Vec<ValueRef> = name.chars().map(|c| {
        C_i32(ccx, char_to_component(c) as i32)
    }).collect();

    if vec.len() == 1 {
        (1, *vec.get(0))
    } else {
        (vec.len(), C_vector(vec.as_slice()))
    }
}

fn char_to_component(c: char) -> uint {
    match c {
        '0'..'9' => {
            (c as uint) - ('0' as uint)
        }
        'a'..'f' => {
            (c as uint) - ('a' as uint)
        }
        'x' => 0,
        'y' => 1,
        'z' => 2,
        'w' => 3,
        _ => fail!("invalid position in shuffle access")
    }
}

fn shuffle_vector(bcx: &Block,
                  mut v1: ValueRef,
                  mut v2: ValueRef,
                  mask: ValueRef) -> ValueRef {

    unsafe {
        let v1_type = llvm::LLVMTypeOf(v1);
        let v1_size = llvm::LLVMGetVectorSize(v1_type) as uint;

        let v2_type = llvm::LLVMTypeOf(v2);
        let v2_size = llvm::LLVMGetVectorSize(v2_type) as uint;

        if v1_size > v2_size {
            v2 = extend_vector(bcx, v2, v2_type, v1_size);
        } else if v2_size > v1_size {
            v1 = extend_vector(bcx, v1, v1_type, v2_size);
        }

        ShuffleVector(bcx, v1, v2, mask)
    }
}

fn extend_vector(bcx: &Block,
                 vec: ValueRef, vec_type: TypeRef,
                 size: uint) -> ValueRef {
    unsafe {
        let other_vec = llvm::LLVMConstNull(vec_type);
        let mask : Vec<ValueRef> = range(0, size).map(|i| C_i32(bcx.ccx(), i as i32)).collect();

        let mask = C_vector(mask.as_slice());

        ShuffleVector(bcx, vec, other_vec, mask)
    }
}
