// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use fmt::parse::*;
use prelude::*;

use io::{Writer,WriterUtil};
use num::ToStrRadix;

pub static printf_desc : ParserDesc<'static> = ParserDesc {
    indicator: '%',
    special_flag: Some('!'),
    flags: ['#', '0', '-', ' ', '+', '\''],
    specifiers: ['d', 'f', 's', '?']
};

// (specifier, method name, whether to pass the numeric flag
pub static printf_handlers: &'static [(char, &'static str, bool)] =
    &[
        ('d', "format_d", true),
        ('f', "format_f", true),
        ('s', "format_s", false),
        ('?', "format_poly", false)
    ];

// unfortunately the cruft in io means not using @Writer is hard, this
// should change when io gets redone
pub trait Formatter {
    fn format(&self, @Writer, Option<int>, Option<int>, &[char]);
}

pub trait IntFormat {
    fn format_d(&self, @Writer, Option<int>, Option<int>, &[char], Option<uint>);
}

pub trait FloatFormat {
    fn format_f(&self, @Writer, Option<int>, Option<int>, &[char], Option<uint>);
}

pub trait StrFormat {
    fn format_s(&self, @Writer, Option<int>, Option<int>, &[char]);
}

/// dummy trait so that every type has the `.format_poly` method
pub trait PolyFormat {
    fn format_poly(&self, @Writer, Option<int>, Option<int>, &[char]);
}
impl<A> PolyFormat for A {
    fn format_poly(&self, w: @Writer, _: Option<int>, _: Option<int>, _: &[char]) {
        ::repr::write_repr(w, self);
    }
}

pub enum Alignment {
    Left, Center, Right
}

pub fn align_field(contents: ~str, width:uint, align:Alignment) -> ~str {
    if contents.len() >= width { return contents; }

    let mut contents = contents;

    let padding = width - contents.len();

    match align {
        Left => {
            for padding.times {
                contents.push_char(' ');
            }
        }
        Center => {
            let pre = padding/2;
            let post = padding-pre;
            for pre.times {
                str::unshift_char(&mut contents, ' ');
            }
            for post.times {
                contents.push_char(' ');
            }
        }
        Right => {
            for padding.times {
                str::unshift_char(&mut contents, ' ');
            }
        }
    }

    contents
}

// completely ignores the spec, and just calls to_str
macro_rules! naive_impl{
    ($trt:ident, $mthd:ident, $ty:ty) => {
        impl $trt for $ty {
            fn $mthd(&self, w: @Writer, width:Option<int>, _prec:Option<int>, flags:&[char]) {
                let str = self.to_str();
                let str = match width {
                    Some(i) => align_field(str, i as uint, Right),
                    None => str
                }
                w.write_str(str);
            }
        }
    }
}

macro_rules! num_impl{
    ($trt:ident, $mthd:ident, $ty:ty) => {
        impl $trt for $ty {
            fn $mthd(&self, w: @Writer,
                     width: Option<int>, _prec: Option<int>,
                     _flags: &[char], base: Option<uint>) {
                let base = match base {
                    Some(i) => i,
                    None => 10
                };
                let str = self.to_str_radix(base);
                let str = match width {
                    Some(i) => align_field(str, i as uint, Right),
                    None => str
                };
                w.write_str(str);
            }
        }
    }
}

macro_rules! df{ ($ty:ty) => { num_impl!(IntFormat, format_d, $ty) } }
macro_rules! ff{ ($ty:ty) => { num_impl!(FloatFormat, format_f, $ty) } }
macro_rules! sf{ ($ty:ty) => { naive_impl!(StrFormat, format_s, $ty) } }

df!{int}  df!{i8} df!{i16} df!{i32} df!{i64}
df!{uint} df!{u8} df!{u16} df!{u32} df!{u64}

ff!{float} ff!{f32} ff!{f64}

// needs char::to_str
// sf!{char}
impl StrFormat for char {
    fn format_s(&self, w: @Writer, _width: Option<int>, _prec: Option<int>, _flags: &[char]) {
        w.write_char(*self)
    }
}
impl<'self> StrFormat for &'self str {
    fn format_s(&self, w: @Writer, _width: Option<int>, _prec: Option<int>, _flags: &[char]) {
        w.write_str(*self)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parse::*;
    use printf;

    #[test]
    fn test_int() {
        let spec = Spec {
            specifier: 'd',
            position: option::None, width: None, precision: None,
            flags: ~[],
            special_flag: false,
            num_arg: option::None
        };

        macro_rules! t(
            ($tst:expr) => {
                assert_eq!(
                    io::with_str_writer(|w| $tst.format_d(w, copy spec)),
                    ~"1")
            }
        );
        t!(1i);
        t!(1i8);
        t!(1i16);
        t!(1i32);
        t!(1i64);

        t!(1u);
        t!(1u8);
        t!(1u16);
        t!(1u32);
        t!(1u64);
    }

    #[test]
    fn test_float() {
        let spec = Spec {
            specifier: 'f',
            position: option::None, width: None, precision: None,
            flags: ~[],
            special_flag: false,
            num_arg: option::None
        };

        macro_rules! t(
            ($tst:expr) => {
                assert!(
                    str::starts_with(
                        io::with_str_writer(|w| $tst.format_f(w, copy spec)),
                        "1.25"))
            }
        );
        t!(1.25f);
        t!(1.25f32);
        t!(1.25f64);
    }

    #[test]
    fn test_str() {
        let spec = Spec {
            specifier: 's',
            position: option::None, width: None, precision: None,
            flags: ~[],
            special_flag: false,
            num_arg: option::None
        };

        macro_rules! t(
            ($tst:expr) => {
                assert_eq!(
                    io::with_str_writer(|w| $tst.format_s(w, copy spec)),
                    ~"x");
            }
        );
        t!("x");
        t!('x');
    }

    #[test]
    fn test_align_none() {
        let a = align_field(~"field", 0, printf::Left);
        assert_eq!(a, ~"field");
    }

    #[test]
    fn test_align_left() {
        let a = align_field(~"field", 10, printf::Left);
        assert_eq!(a, ~"field     ");
    }

    #[test]
    fn test_align_center() {
        let a = align_field(~"field", 10, printf::Center);
        assert_eq!(a, ~"  field   ");
    }

    #[test]
    fn test_align_right() {
        let a = align_field(~"field", 10, printf::Right);
        assert_eq!(a, ~"     field");
    }
}
