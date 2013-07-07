// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use abi::AbiSet;
use ast::*;
use ast;
use ast_util::{stmt_id};
use ast_util;
use codemap;
use diagnostic::span_handler;
use parse::token::ident_interner;
use print::pprust;
use visit;
use visit_new::{Visitor,FnKind};
use syntax::parse::token::special_idents;

use std::cmp;
use std::hashmap::HashMap;
use std::vec;

pub enum path_elt {
    path_mod(ident),
    path_name(ident)
}

impl cmp::Eq for path_elt {
    fn eq(&self, other: &path_elt) -> bool {
        match (*self) {
            path_mod(e0a) => {
                match (*other) {
                    path_mod(e0b) => e0a == e0b,
                    _ => false
                }
            }
            path_name(e0a) => {
                match (*other) {
                    path_name(e0b) => e0a == e0b,
                    _ => false
                }
            }
        }
    }
    fn ne(&self, other: &path_elt) -> bool { !(*self).eq(other) }
}

pub type path = ~[path_elt];

pub fn path_to_str_with_sep(p: &[path_elt], sep: &str, itr: @ident_interner)
                         -> ~str {
    let strs = do p.map |e| {
        match *e {
          path_mod(s) => itr.get(s.name),
          path_name(s) => itr.get(s.name)
        }
    };
    strs.connect(sep)
}

pub fn path_ident_to_str(p: &path, i: ident, itr: @ident_interner) -> ~str {
    if p.is_empty() {
        //FIXME /* FIXME (#2543) */ copy *i
        itr.get(i.name).to_owned()
    } else {
        fmt!("%s::%s", path_to_str(*p, itr), itr.get(i.name))
    }
}

pub fn path_to_str(p: &[path_elt], itr: @ident_interner) -> ~str {
    path_to_str_with_sep(p, "::", itr)
}

pub fn path_elt_to_str(pe: path_elt, itr: @ident_interner) -> ~str {
    match pe {
        path_mod(s) => itr.get(s.name).to_owned(),
        path_name(s) => itr.get(s.name).to_owned()
    }
}

pub enum AstNode<'self> {
    NodeItem(&'self item, path),
    NodeForeignItem(&'self foreign_item, AbiSet, visibility, path),
    NodeTraitMethod(&'self trait_method, def_id /* trait did */,
                     path /* path to the trait */),
    NodeMethod(&'self method, def_id /* impl did */, path /* path to the impl */),
    NodeVariant(&'self variant, &'self item, path),
    NodeExpr(&'self expr),
    NodeStmt(&'self stmt),
    NodeArg,
    NodeLocal(ident),
    NodeBlock(&'self blk),
    NodeStructCtor(&'self struct_def, &'self item, path),
    NodeCalleeScope(&'self expr)
}

pub type Map<'self> = HashMap<node_id, AstNode<'self>>;

struct Ctx<'self> {
    map: Map<'self>,
    path: path,
    diag: @span_handler,
}


pub fn map_crate<'r>(diag: @span_handler, c: &'r crate) -> Map<'r> {
    let mut ctx = Ctx {
        map: HashMap::new(),
        path: ~[],
        diag: diag,
    };
    ctx.visit_crate(c);
    ctx.map
}

impl<'self> Ctx<'self> {
    #[inline]
    priv fn life<T>(&self, ptr: &T) -> &'self T {
        use std::cast::transmute;

        /*
         * This uses transmute because we need references to the items in the crate, but threading
         * the lifetimes through would require a whole new visitor, just for this case.
         *
         * In this instance, the lifetime assurance is provided by the map_crate function, so
         * transmuting the region like this is safe. If somebody want to make a new visitor though,
         * go for it.
         */
        unsafe {
            transmute(ptr)
        }
    }

    pub fn extend(&self, elt: ident) -> path {
        self.path + &[path_name(elt)]
    }

    pub fn map_method(&mut self, impl_did: def_id, impl_path: path, m: &'self method) {
        self.map.insert(m.id, NodeMethod(m, impl_did, impl_path));
        self.map.insert(m.self_id, NodeLocal(special_idents::self_));
    }

}

impl<'self> Visitor for Ctx<'self> {

    pub fn visit_item(&mut self, i: &item) {
        let item_path = copy self.path;
        let i = self.life(i);
        self.map.insert(i.id, NodeItem(i, copy item_path));

        match i.node {
            item_impl(_, _, _, ref ms) => {
                let impl_did = ast_util::local_def(i.id);
                for ms.iter().advance |&m| {
                    let p = self.extend(i.ident);
                    let m = self.life(m);
                    self.map_method(impl_did, p, m);
                }
            }
            item_enum(ref enum_definition, _) => {
                for enum_definition.variants.iter().advance |v| {
                    let p = self.extend(i.ident);
                    self.map.insert(v.node.id, NodeVariant(v, i, p));
                }
            }
            item_foreign_mod(ref nm) => {
                for nm.items.iter().advance |&nitem| {
                    // Compute the visibility for this native item.
                    let visibility = match nitem.vis {
                        public => public,
                        private => private,
                        inherited => i.vis
                    };

                    let nitem = self.life(nitem);
                    let  p = if nm.sort == ast::named {
                        self.extend(i.ident)
                    } else {
                        // Anonymous extern mods go in the parent scope
                        copy self.path
                    };

                    self.map.insert(nitem.id,
                        NodeForeignItem(
                            nitem,
                            nm.abis,
                            visibility,
                            p)
                    );
                }
            }
            item_struct(@ref struct_def, _) if struct_def.ctor_id.is_some() => {
                let ctor_id = struct_def.ctor_id.get();
                let path = self.extend(i.ident);
                self.map.insert(ctor_id,
                                NodeStructCtor(struct_def, i, path));
            }
            item_trait(_, ref traits, ref methods) => {
                for traits.iter().advance |p| {
                    self.map.insert(p.ref_id, NodeItem(i, copy item_path));
                }
                for methods.iter().advance |tm| {
                    let id = ast_util::trait_method_to_ty_method(tm).id;
                    let d_id = ast_util::local_def(i.id);
                    self.map.insert(id, NodeTraitMethod(tm, d_id, copy item_path));
                }
            }
            _ => ()
        }

        match i.node {
            item_mod(_) | item_foreign_mod(_) => {
                self.path.push(path_mod(i.ident));
            }
            _ => self.path.push(path_name(i.ident))
        }
        self.visit_item_contents(i);
        self.path.pop();
    }

    pub fn visit_expr(&mut self, ex: &expr) {
        let ex = self.life(ex);
        self.map.insert(ex.id, NodeExpr(ex));

        // Expressions which are or might be calls:
        let r = ex.get_callee_id();
        for r.iter().advance |&callee_id| {
            self.map.insert(callee_id, NodeCalleeScope(ex));
        }

        self.visit_expr_contents(ex);
    }

    pub fn visit_stmt(&mut self, s: &stmt) {
        let s = self.life(s);
        self.map.insert(stmt_id(s), NodeStmt(s));
        self.visit_stmt_contents(s);
    }

    pub fn visit_fn(&mut self, fk: &FnKind, decl: &fn_decl, body: &blk,
                    sp: codemap::span, id: node_id) {
        for decl.inputs.iter().advance |a| {
            self.map.insert(a.id, NodeArg);
        }
        self.visit_fn_contents(fk, decl, body, sp, id);
    }

    pub fn visit_block(&mut self, blk: &blk) {
        let blk = self.life(blk);
        self.map.insert(blk.node.id, NodeBlock(blk));
        self.visit_block_contents(blk);
    }

    pub fn visit_pat(&mut self, pat: &pat) {
        match pat.node {
            pat_ident(_, ref path, _) => {
                self.map.insert(pat.id, NodeLocal(ast_util::path_to_ident(path)));
            }
            _ => ()
        }

        self.visit_pat_contents(pat);
    }
}

// Used for items loaded from external crate that are being inlined into this
// crate.  The `path` should be the path to the item but should not include
// the item itself.
pub fn map_decoded_item<'r>(diag: @span_handler,
                        map: Map<'r>,
                        path: path,
                        ii: &'r inlined_item) -> Map<'r> {
    // I believe it is ok for the local IDs of inlined items from other crates
    // to overlap with the local ids from this crate, so just generate the ids
    // starting from 0.  (In particular, I think these ids are only used in
    // alias analysis, which we will not be running on the inlined items, and
    // even if we did I think it only needs an ordering between local
    // variables that are simultaneously in scope).
    let mut cx = Ctx {
        map: map,
        path: copy path,
        diag: diag,
    };

    // methods get added to the AST map when their impl is visited.  Since we
    // don't decode and instantiate the impl, but just the method, we have to
    // add it to the table now:
    match *ii {
        ii_item(*) => { /* fallthrough */ }
        ii_foreign(@ref i) => {
            cx.map.insert(i.id, NodeForeignItem(i, AbiSet::Intrinsic(),
                                                  i.vis,    // Wrong but OK
                                                  path));
        }
        ii_method(impl_did, @ref m) => {
            cx.map_method(impl_did, path, m);
        }
    }

    // visit the item / method contents and add those to the map:
    ii.accept(&mut cx);

    cx.map
}


pub fn node_id_to_str(map: Map, id: node_id, itr: @ident_interner) -> ~str {
    match map.find(&id) {
      None => {
        fmt!("unknown node (id=%d)", id)
      }
      Some(&NodeItem(item, ref path)) => {
        let path_str = path_ident_to_str(path, item.ident, itr);
        let item_str = match item.node {
          item_static(*) => ~"static",
          item_fn(*) => ~"fn",
          item_mod(*) => ~"mod",
          item_foreign_mod(*) => ~"foreign mod",
          item_ty(*) => ~"ty",
          item_enum(*) => ~"enum",
          item_struct(*) => ~"struct",
          item_trait(*) => ~"trait",
          item_impl(*) => ~"impl",
          item_mac(*) => ~"macro"
        };
        fmt!("%s %s (id=%?)", item_str, path_str, id)
      }
      Some(&NodeForeignItem(item, abi, _, ref path)) => {
        fmt!("foreign item %s with abi %? (id=%?)",
             path_ident_to_str(path, item.ident, itr), abi, id)
      }
      Some(&NodeMethod(m, _, ref path)) => {
        fmt!("method %s in %s (id=%?)",
             itr.get(m.ident.name), path_to_str(*path, itr), id)
      }
      Some(&NodeTraitMethod(ref tm, _, ref path)) => {
        let m = ast_util::trait_method_to_ty_method(&**tm);
        fmt!("method %s in %s (id=%?)",
             itr.get(m.ident.name), path_to_str(*path, itr), id)
      }
      Some(&NodeVariant(ref variant, _, ref path)) => {
        fmt!("variant %s in %s (id=%?)",
             itr.get(variant.node.name.name), path_to_str(*path, itr), id)
      }
      Some(&NodeExpr(expr)) => {
        fmt!("expr %s (id=%?)", pprust::expr_to_str(expr, itr), id)
      }
      Some(&NodeCalleeScope(expr)) => {
        fmt!("callee_scope %s (id=%?)", pprust::expr_to_str(expr, itr), id)
      }
      Some(&NodeStmt(stmt)) => {
        fmt!("stmt %s (id=%?)",
             pprust::stmt_to_str(stmt, itr), id)
      }
      Some(&NodeArg) => {
        fmt!("arg (id=%?)", id)
      }
      Some(&NodeLocal(ident)) => {
        fmt!("local (id=%?, name=%s)", id, itr.get(ident.name))
      }
      Some(&NodeBlock(_)) => {
        fmt!("block")
      }
      Some(&NodeStructCtor(*)) => {
        fmt!("struct_ctor")
      }
    }
}

pub fn node_item_query<Result>(items: Map, id: node_id,
                               query: &fn(&item) -> Result,
                               error_msg: ~str) -> Result {
    match items.find(&id) {
        Some(&NodeItem(it, _)) => query(it),
        _ => fail!(error_msg)
    }
}
