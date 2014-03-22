// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use ast;
use codemap::Span;
use ext::base;
use ext::base::*;
use parse;
use parse::token;

pub fn expand_simd(cx: &mut ExtCtxt, sp: Span, tts: &[ast::TokenTree])
    -> base::MacResult {

    let mut p = parse::new_parser_from_tts(cx.parse_sess(), cx.cfg(),
        tts.iter().map(|x| (*x).clone())
                  .collect());

    let expr = cx.expand_expr(p.parse_expr());
    if p.token == token::COMMA && p.look_ahead(1, |t| *t == token::DOTDOT) {
        p.bump(); p.bump();
        let count = p.parse_expr();
        MRExpr(@ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprSimdRepeat(expr, count),
            span: sp
        })
    } else {
        let mut exprs = Vec::new();
        exprs.push(expr);
        while p.token != token::EOF {
            p.expect(&token::COMMA);
            exprs.push(cx.expand_expr(p.parse_expr()));
        }

        MRExpr(@ast::Expr {
            id: ast::DUMMY_NODE_ID,
            node: ast::ExprSimd(exprs),
            span: sp
        })
    }
}
