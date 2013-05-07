// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use ast;
use codemap::span;
use ext::base::*;
use ext::base;
use ext::build;
use ext::build::*;

use core::fmt;

pub fn expand_syntax_ext(cx: @ext_ctxt, sp: span, tts: &[ast::token_tree])
    -> base::MacResult {
    match get_exprs_from_tts(cx, tts) {
    [] | [_] => cx.span_fatal(sp, "writef! takes at least 2 arguments."),
    [writer, rawfmt, .. args] => {
        let fmtstr = expr_to_str(cx, rawfmt,
                  ~"Format string must be a string literal");
        let mut parser = fmt::parse::Parser::new(fmtstr, &fmt::printf::printf_desc);
        let mut count = 0;
        let mut seen_positional = false;

        let next_arg = || {
            if count >= args.len() {
                cx.span_fatal(sp, ~"too few arguments to writef!");
            }
            if seen_positional {
                cx.span_err(sp, ~"ERROR MESSAGE");
            }
            let a = args[count];
            count += 1;
            a
        };
        let positional_arg = |i: int| {
            if i < 0 || i >= args.len() as int {
                cx.span_fatal(sp, fmt!("positional argument %d doesn't exist", i));
            }
            seen_positional = true;
            args[i]
        };
        let field_to_option = |f: fmt::parse::Field| {
            match f {
                fmt::parse::None => None,
                fmt::parse::Value(i) => Some(build::mk_int(cx, sp, i)),
                fmt::parse::Next => {
                    Some(next_arg())
                }
                fmt::parse::Arg(i) => {
                    Some(positional_arg(i))
                }
            }
        };

        let write_str = cx.ident_of(~"write_str");

        // save writer to a local variable, so that it is only evaluated once
        let wr_var_ident = cx.ident_of(~"__writer");
        let wr_var_expr = build::mk_path(cx, sp, ~[wr_var_ident]);
        let set_wr_var = build::mk_local(cx, sp, false,
                                         wr_var_ident, writer);
        let mut stmts = ~[set_wr_var];

        loop {
            match parser.next_item() {
            fmt::parse::Place(spc) => {
                let arg = match spc.position {
                None => {
                    next_arg()
                }
                Some(i) => {
                    positional_arg(i)
                }
                };

                let specifier_info_idx =
                    vec::position(fmt::printf::printf_handlers,
                                  |&(sp, _, _)| sp == spc.specifier);

                let (_, meth_name, use_num_arg) = match specifier_info_idx {
                    None => cx.span_fatal(sp, fmt!("Unrecognised specifier `%c`", spc.specifier)),
                    Some(i) => {
                        fmt::printf::printf_handlers[i]
                    }
                };

                if !use_num_arg && spc.num_arg.is_some() {
                    cx.span_warn(sp,
                                 fmt!("<%d> flag has no effect with specifier `%c`",
                                      spc.num_arg.get(), spc.specifier));
                }

                // this should be factored into core::fmt
                let width = mk_option(cx, sp,
                                      field_to_option(spc.width));
                let precision = mk_option(cx, sp,
                                          field_to_option(spc.precision));
                let flags = mk_base_vec_e(cx, arg.span,
                                          spc.flags.map(|&c|
                                                        build::mk_lit(
                                                            cx, sp,
                                                            ast::lit_int(c as i64,
                                                                         ast::ty_char))));

                let mut call_args = ~[wr_var_expr, width, precision, flags];
                if use_num_arg {
                    let opt = do spc.num_arg.map |&x| {
                        if x < 2 {
                            cx.span_fatal(sp,
                                          fmt!("base argument `%d` must be at least 2", x))
                        }
                        build::mk_uint(cx, sp, x as uint)
                    };
                    call_args.push(mk_option(cx, sp, opt));
                }

                let item = build::mk_method_call(cx, arg.span,
                                                 arg,
                                                 cx.ident_of(meth_name.to_owned()),
                                                 call_args);
                stmts.push(build::mk_stmt(cx, arg.span, item));
            }
            fmt::parse::Raw(str) => {
                let str = build::mk_base_str(cx, sp, str.to_owned());
                let item = build::mk_method_call(cx, sp,
                                  wr_var_expr, write_str,
                                  ~[str]);
                stmts.push(build::mk_stmt(cx, sp, item));
            }
            fmt::parse::End => break,
            _ => {
                // XXX: give position within format string
                cx.span_fatal(sp, fmt!("error parsing format string"));
            }
            }
        }
        if count != args.len() {
        cx.span_warn(sp,
                   fmt!("Incorrect number of arguments to writef!: %u but expecting %u",
                        args.len(), count));
        }
        let blck = build::mk_block(cx, sp, ~[], stmts, None);

        MRExpr(blck)
    }
    }
}


fn mk_option(cx: @ext_ctxt, sp: span, opt: Option<@ast::expr>) -> @ast::expr {
    match opt {
        None => build::mk_path_global(cx, sp, ~[cx.ident_of(~"core"),
                                                cx.ident_of(~"option"),
                                                cx.ident_of(~"None")]),
        Some(expr) => build::mk_call_global(cx, sp,
                                           ~[cx.ident_of(~"core"),
                                             cx.ident_of(~"option"),
                                             cx.ident_of(~"Some")],
                                            ~[expr])
    }
}
