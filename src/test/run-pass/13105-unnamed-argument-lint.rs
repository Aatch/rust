// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


// Regression test for issue #13105

#[deny(uppercase_variables)];

trait Foo {
    // Good
    fn foo(&self);

    // Good
    fn bar(u8);

    // Good
    fn baz(&self) {

    }

    // ICE
    fn quux(u8) {

    }
}

fn main() {

}
