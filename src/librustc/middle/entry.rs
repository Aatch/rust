// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


use driver::session;
use driver::session::Session;
use syntax::parse::token::special_idents;
use syntax::ast::{crate, node_id, item, item_fn};
use syntax::codemap::span;
use syntax::visit_new::Visitor;
use syntax::attr::{attrs_contains_name};
use syntax::ast_map;
use std::util;

struct EntryContext<'self> {
    session: Session,

    ast_map: &'self ast_map::Map<'self>,

    // The top-level function called 'main'
    main_fn: Option<(node_id, span)>,

    // The function that has attribute named 'main'
    attr_main_fn: Option<(node_id, span)>,

    // The function that has the attribute 'start' on it
    start_fn: Option<(node_id, span)>,

    // The functions that one might think are 'main' but aren't, e.g.
    // main functions not defined at the top level. For diagnostics.
    non_main_fns: ~[(node_id, span)],
}

impl<'self> Visitor for EntryContext<'self> {
    pub fn visit_item(&mut self, item: &item) {
        match item.node {
            item_fn(*) => {
                if item.ident == special_idents::main {
                    match self.ast_map.find(&item.id) {
                        Some(&ast_map::NodeItem(_, ref path)) => {
                            if path.len() == 0 {
                                // This is a top-level function so can be 'main'
                                if self.main_fn.is_none() {
                                    self.main_fn = Some((item.id, item.span));
                                } else {
                                    self.session.span_err(item.span,
                                        "multiple 'main' functions");
                                }
                            } else {
                                // This isn't main
                                self.non_main_fns.push((item.id, item.span));
                            }
                        }
                        _ => util::unreachable()
                    }
                }

                if attrs_contains_name(item.attrs, "main") {
                    if self.attr_main_fn.is_none() {
                        self.attr_main_fn = Some((item.id, item.span));
                    } else {
                        self.session.span_err(item.span,
                            "multiple 'main' functions");
                    }
                }

                if attrs_contains_name(item.attrs, "start") {
                    if self.start_fn.is_none() {
                        self.start_fn = Some((item.id, item.span));
                    } else {
                        self.session.span_err(item.span,
                            "multiple 'start' functions");
                    }
                }
            }
            _ => ()
        }

        self.visit_item_contents(item);
    }

}

impl<'self> EntryContext<'self> {
    fn configure_main(&mut self) {
        if self.start_fn.is_some() {
            *self.session.entry_fn = self.start_fn;
            *self.session.entry_type = Some(session::EntryStart);
        } else if self.attr_main_fn.is_some() {
            *self.session.entry_fn = self.attr_main_fn;
            *self.session.entry_type = Some(session::EntryMain);
        } else if self.main_fn.is_some() {
            *self.session.entry_fn = self.main_fn;
            *self.session.entry_type = Some(session::EntryMain);
        } else {
            if !*self.session.building_library {
                // No main function
                self.session.err("main function not found");
                if !self.non_main_fns.is_empty() {
                    // There were some functions named 'main' though. Try to give the user a hint.
                    self.session.note("the main function must be defined at the crate level \
                                       but you have one or more functions named 'main' that are not \
                                       defined at the crate level. Either move the definition or \
                                       attach the `#[main]` attribute to override this behavior.");
                    for self.non_main_fns.iter().advance |&(_, span)| {
                        self.session.span_note(span, "here is a function named 'main'");
                    }
                }
                self.session.abort_if_errors();
            } else {
                // If we *are* building a library, then we're on android where we still might
                // optionally want to translate main $4404
                assert_eq!(self.session.targ_cfg.os, session::os_android);
            }
        }
    }

}


pub fn find_entry_point<'r>(session: Session, crate: &'r crate, ast_map: &'r ast_map::Map<'r>) {

    // FIXME #4404 android JNI hacks
    if *session.building_library &&
        session.targ_cfg.os != session::os_android {
        // No need to find a main function
        return;
    }

    let mut ctxt = EntryContext {
        session: session,
        ast_map: ast_map,
        main_fn: None,
        attr_main_fn: None,
        start_fn: None,
        non_main_fns: ~[],
    };

    ctxt.visit_crate(crate);
    ctxt.configure_main();
}
