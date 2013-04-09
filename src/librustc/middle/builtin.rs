// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*

 builtin.rs: Handle built-in functions and intrinsics.

 Some functions are better implemented in the compiler as
 "built-ins", functions that have no real Rust-level representation
 or need to expose something that can only be done by using LLVM
 intrinsics or where assembly-level implementation is too low-level
 to be manageable.
 */

use core::prelude::*;

use middle::trans;
use middle::trans::common::{CrateContext};
use middle::ty;

use syntax::ast;
use syntax::ast_util::{local_def};

// Slightly complicated macro for defining the names of all
// the built-ins. It only defines the names, and the mapping
// of the names to the enum, but that is enough to allow the
// the compiler to warn us when we have missed a new builtin.
macro_rules! define_builtins(
    ($($name:ident),+) => (
        pub mod b { //This wraps because you can't make multiple items from one macro call
            use core::prelude::*;
            use driver::session;
            use syntax::ast;
            pub enum BuiltIn {
                $($name),+
            }
            pub fn builtin_from_ident(sess: session::Session, id: ast::ident) ->
                Option<BuiltIn> {
                let ident_str = sess.str_of(id);
                // Using an if-else chain here because using `match` doesn't
                // play nice with the macro expander.
                $(if str::eq_slice(ident_str, stringify!($name)) {
                    Some($name)
                } else)+ {
                    sess.err(fmt!("Unknown built-in %s", ident_str));
                    None
                }
            }
        }
    )
)

// Now use our shiny macro!
define_builtins!(
    atomic_cxchg,
    atomic_cxchg_acq,
    atomic_cxchg_rel
)

pub enum TranslationType {
    Wrapper,
    CallSite
}

pub struct BuiltInDesc {
    bi: b::BuiltIn,
    trans_ty: TranslationType,
    nptys: uint,
    inputs: ~[ty::arg],
    output: ty::t,
    use_flags: trans::type_use::type_uses
}

fn mk_desc(ccx: &CrateContext, bi:b::BuiltIn) -> ~BuiltInDesc {
    fn param(ccx: &CrateContext, n: uint) -> ty::t {
        ty::mk_param(ccx.tcx, n, local_def(0))
    };
    fn arg(m: ast::rmode, ty: ty::t) -> ty::arg {
        ty::arg {mode: ast::expl(m), ty: ty}
    };
    let tcx = ccx.tcx;
    ~match bi {
        b::atomic_cxchg => BuiltInDesc {
            bi:bi, trans_ty:Wrapper, nptys: 0,
            inputs: ~[
                arg(ast::by_copy, ty::mk_mut_rptr(tcx, ty::re_bound(ty::br_anon(0)),
                                                  ty::mk_int(tcx))),
                arg(ast::by_copy, ty::mk_int(tcx)),
                arg(ast::by_copy, ty::mk_int(tcx))],
            output: ty::mk_int(tcx), use_flags: 0 },
        _ => fail!()
    }
}
