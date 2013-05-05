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
                  expr_to_str);
        let parser = fmt::parse::Parser::new(fmtstr, &fmt::printf::printf_desc);
        let mut count = 0;

        let runtime_spec_id = ~[
        cx.ident_of("core"),
        cx.ident_of("fmt"),
        cx.ident_of("RTSpec") ];
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

                // this should be factored into core::fmt
                let method;
                match spc.specifier {
                's' => {
                    match spc.num_arg {
                    Some(i) =>  {
                        cx.warn(
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
                                     str_call);
                    push(build::mk_stmt(cx, sp, item));
                    loop;
                }
                }
                // XXX: runtime spec
                let runtime_spec = build::mk_global_struct_e(cx, sp,
                                     copy runtime_spec_id,
                                     ~[] );
                let item = build::mk_method_call(cx, sp,
                                 arg, cx.ident_of(method),
                                 ~[ writer, runtime_spec ]);
                push(build::mk_stmt(cx, sp, item));
            }
            fmt::parse::Raw(str) => {
                let str = build::mk_base_str(cx, sp, str.to_owned());
                let item = build::mk_method_call(cx, sp,
                                  writer, write_str,
                                  str);
                push(build::mk_stmt(cx, sp, item));
            }
            fmt::parse::Error {msg:msg, pos:pos} => {
                // XXX: give position within format string
                cx.span_fatal(sp, fmt!("error parsing format string: %s", msg));
            }
            fmt::parse::End => break,
            }
        }
        };
        if count != args.len() {
        cx.span_warn(sp,
                 fmt!("Incorrect number of arguments to writef!: %d but expecting %d",
                  args.len(), count));
        }
        let blck = build::mk_block(cx, sp, ~[], stmts, None);

        MRExpr(blck)
    }
    }
}
