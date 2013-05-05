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

        let write_str = cx.ident_of(~"write_str");
        let stmts = do vec::build |push| {
        loop {
            match parser.next_item() {
            fmt::parse::Place(spc) => {
                let arg = match spc.position {
                None => {
                    if count >= args.len() {
                    cx.span_fatal(sp, "too few arguments to writef!");
                    }
                    let a = args[count];
                    count += 1;
                    a
                }
                Some(i) => {
                    args[i]
                }
                };

                let opt_none = mk_path_global(cx, arg.span, ~[
                    cx.ident_of(~"core"),
                    cx.ident_of(~"option"),
                    cx.ident_of(~"None"),
                ]);

                let flags = mk_base_vec_e(cx, arg.span, ~[]);

                // this should be factored into core::fmt
                let method;
                let mut call_args = ~[ writer, copy opt_none, opt_none, flags ];

                match spc.specifier {
                's' => {
                    match spc.num_arg {
                    Some(i) =>  {
                        cx.span_warn(
                        sp,
                        fmt!("`<%d>` flag has no effect with specifier `s`",
                             i))
                    }
                    None => {}
                    }

                    method = ~"format_s";
                }
                'd' => {
                    method = ~"format_d";
                    call_args.push(mk_uint(cx, arg.span, 10));
                }
                'f' => {
                    method = ~"format_f";
                }
                '?' => {
                    let str_call = mk_call_global(
                    cx, sp,
                    ~[
                        cx.ident_of(~"core"),
                        cx.ident_of(~"sys"),
                        cx.ident_of(~"log_str") ],
                    ~[ arg ]);
                    let item = build::mk_method_call(cx, sp,
                                     writer, write_str,
                                     ~[str_call]);
                    push(build::mk_stmt(cx, arg.span, item));
                    loop;
                }
                _ => fail!("Unknown specifier")
                }
                let item = build::mk_method_call(cx, arg.span,
                                 arg, cx.ident_of(method),
                                 call_args);
                push(build::mk_stmt(cx, arg.span, item));
            }
            fmt::parse::Raw(str) => {
                let str = build::mk_base_str(cx, sp, str.to_owned());
                let item = build::mk_method_call(cx, sp,
                                  writer, write_str,
                                  ~[str]);
                push(build::mk_stmt(cx, sp, item));
            }
            fmt::parse::End => break,
            _ => {
                // XXX: give position within format string
                cx.span_fatal(sp, fmt!("error parsing format string"));
            }
            }
        }
        };
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
