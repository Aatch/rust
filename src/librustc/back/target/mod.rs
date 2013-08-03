// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use util::triple;
use util::triple::*;
use driver::driver::early_error;

use syntax::{ast,attr};
use syntax::attr::{AttrMetaMethods};
use syntax::diagnostic;

mod targets;

pub trait Target {
    /// Get the triple for this target
    pub fn triple(&self) -> triple::Triple;

    // Target data types
    /// The native int type
    pub fn int_type(&self) -> ast::int_ty {
        let triple = self.triple();
        if triple.arch.pointer_size() == 64 {
            ast::ty_i64
        } else if triple.arch.pointer_size() == 16 {
            ast::ty_i16
        } else {
            ast::ty_i32
        }
    }
    /// The native uint type
    pub fn uint_type(&self) -> ast::uint_ty {
        let triple = self.triple();
        if triple.arch.pointer_size() == 64 {
            ast::ty_u64
        } else if triple.arch.pointer_size() == 16 {
            ast::ty_u16
        } else {
            ast::ty_u32
        }
    }
    /// The native float type
    pub fn float_type(&self) -> ast::float_ty {
        // Current behaviour is always f64 (?)
        ast::ty_f64
    }

    /// The data layout string
    pub fn data_layout<'r>(&'r self) -> &'r str;
    /// The name of the section metadata is stored in
    pub fn meta_sect_name<'r>(&'r self) -> &'r str {
        let triple = self.triple();

        if triple.is_binfmt_macho() {
            "__DATA,__note.rustc"
        } else {
            ".note.rustc"
        }
    }

    pub fn add_cfgs(&self, cfg: &mut ast::CrateConfig) {
        self.triple().add_to_cfg(cfg);
    }

    pub fn add_cc_args(&self, args: &mut ~[~str]) { }
}

fn add_attr_item(cfg: &mut ast::CrateConfig, name: @str) {
    if !cfg.iter().any(|mi| mi.name() == name) {
        cfg.push(attr::mk_word_item(name));
    }
}
fn add_attr_name_value_str(cfg: &mut ast::CrateConfig, name: @str, val: @str) {
    if !cfg.iter().any(|mi| mi.name() == name) {
        cfg.push(attr::mk_name_value_item_str(name, val));
    }
}

macro_rules! add_cfg(
    ($cfg:expr, $name:expr) => (
        add_attr_item($cfg, $name)
    );
    ($cfg:expr, $name:expr, $val:expr) => (
        add_attr_name_value_str($cfg, $name, $val)
    )
)

impl triple::Triple {
    pub fn add_to_cfg(self, cfg: &mut ast::CrateConfig) {
        // Each triple component has some fixed things it adds
        self.arch.add_to_cfg(cfg);
        self.vendor.add_to_cfg(cfg);
        self.os.add_to_cfg(cfg);
        self.env.add_to_cfg(cfg);

        if self.is_windows() {
            add_cfg!(cfg, @"target_libc", @"msvcrt.dll");
        } else if self.is_macosx() {
            add_cfg!(cfg, @"target_libc", @"libc.dylib");
        } else {
            add_cfg!(cfg, @"target_libc", @"libc.so");
        }
    }
}

impl triple::Arch {
    pub fn add_to_cfg(self, cfg: &mut ast::CrateConfig) {
        match self {
            Arm(_)          => add_cfg!(cfg, @"target_endian", @"little"),
            AArch64         => add_cfg!(cfg, @"target_endian", @"little"),
            Hexagon         => add_cfg!(cfg, @"target_endian", @"little"),
            PPC64LE         => add_cfg!(cfg, @"target_endian", @"little"),
            MipsEL          => add_cfg!(cfg, @"target_endian", @"little"),
            Mips64EL        => add_cfg!(cfg, @"target_endian", @"little"),
            MSP430          => add_cfg!(cfg, @"target_endian", @"little"),
            X86             => add_cfg!(cfg, @"target_endian", @"little"),
            X86_64          => add_cfg!(cfg, @"target_endian", @"little"),
            XCore           => add_cfg!(cfg, @"target_endian", @"little"),
            NVPTX           => add_cfg!(cfg, @"target_endian", @"little"),
            NVPTX64         => add_cfg!(cfg, @"target_endian", @"little"),
            LE32            => add_cfg!(cfg, @"target_endian", @"little"),
            SPIR            => add_cfg!(cfg, @"target_endian", @"little"),
            SPIR64          => add_cfg!(cfg, @"target_endian", @"little"),
            Mips            => add_cfg!(cfg, @"target_endian", @"big"),
            Mips64          => add_cfg!(cfg, @"target_endian", @"big"),
            PPC             => add_cfg!(cfg, @"target_endian", @"big"),
            PPC64           => add_cfg!(cfg, @"target_endian", @"big"),
            R600            => add_cfg!(cfg, @"target_endian", @"big"),
            Sparc           => add_cfg!(cfg, @"target_endian", @"big"),
            Sparcv9         => add_cfg!(cfg, @"target_endian", @"big"),
            SystemZ         => add_cfg!(cfg, @"target_endian", @"big"),
            TCE             => add_cfg!(cfg, @"target_endian", @"big"),
            Thumb(_)        => add_cfg!(cfg, @"target_endian", @"big"),
            _               => add_cfg!(cfg, @"target_endian", @"big") //default to big
        }

        match self.arch_prefix() {
            Some(prefix) => add_cfg!(cfg, @"target_arch_family", prefix.to_managed()),
            None => ()
        }

        add_cfg!(cfg, @"target_arch", self.as_str().to_managed());
        // If it was added above, then it won't be added here.
        add_cfg!(cfg, @"target_arch_family", self.as_str().to_managed());

        add_cfg!(cfg, @"target_word_size", self.pointer_size().to_str().to_managed());
        match self {
            Arm(v) | Thumb(v) => add_cfg!(cfg, @"target_arch_version", v.to_str().to_managed()),
            _ => ()
        }
    }
}

impl triple::Vendor {
    pub fn add_to_cfg(self, cfg: &mut ast::CrateConfig) {
        add_cfg!(cfg, @"target_vendor", self.as_str().to_managed());
    }
}

impl triple::OSType {
    pub fn add_to_cfg(self, cfg: &mut ast::CrateConfig) {
        add_cfg!(cfg, @"target_os", self.as_str().to_managed());
        match self {
            Win32 | MinGW32 | Cygwin => add_cfg!(cfg, @"windows"),
            MacOSX | Linux | FreeBSD
          | KFreeBSD | OpenBSD | NetBSD
          | Solaris => add_cfg!(cfg, @"unix"),
          _ => ()
        }
    }
}

impl triple::EnvironmentType {
    pub fn add_to_cfg(self, cfg: &mut ast::CrateConfig) {
        add_cfg!(cfg, @"target_environment", self.as_str().to_managed());
    }
}

pub fn get_target(triple: &str, demitter: diagnostic::Emitter) -> ~Target {
    let triple = triple::Triple::from_triple(triple);

    let os = triple.os;
    match triple.arch {
        Arm(_) | Thumb(_) => {
            ~targets::Arm::new(triple) as ~Target
        }
        Mips => {
            ~targets::Mips::new(triple) as ~Target
        }
        Mips64 => {
            ~targets::Mips64::new(triple) as ~Target
        }
        MipsEL => {
            ~targets::Mips::new(triple) as ~Target
        }
        Mips64EL => {
            ~targets::Mips64::new(triple) as ~Target
        }
        X86 => {
            ~targets::X86::new(triple) as ~Target
        }
        X86_64 => {
            ~targets::X86_64::new(triple) as ~Target
        }
        LE32 => {
            ~targets::LE32::new(triple) as ~Target
        }
        a => early_error(demitter, fmt!("Unsupported architecture '%s'", a.as_str()))
    }
}
