// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

macro_rules! my_mac (
    ( [ $($name:ident : $val:expr),* ]) => (
        ~[$((stringify!($name), $val)),*]
    )
)

fn main(){
    trace_macros!(true);

    let _ : ~[(~str,uint)] = my_mac!([]);
    my_mac!([foo: 1]);
    my_mac!([bar: 2, baz: 3]);
}
