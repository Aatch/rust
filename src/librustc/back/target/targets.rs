
use util::triple::*;
use super::Target;

pub struct Arm {
    triple: Triple
}

pub struct Mips {
    triple: Triple
}

pub struct Mips64 {
    triple: Triple
}

pub struct X86 {
    triple: Triple
}

pub struct X86_64 {
    triple: Triple
}

pub struct LE32 {
    triple: Triple
}

impl Arm {
    pub fn new(triple: Triple) -> Arm {
        Arm { triple: triple }
    }
}

impl Mips {
    pub fn new(triple: Triple) -> Mips {
        Mips { triple: triple }
    }
}

impl Mips64 {
    pub fn new(triple: Triple) -> Mips64 {
        Mips64 { triple: triple }
    }
}

impl X86 {
    pub fn new(triple: Triple) -> X86 {
        X86 { triple: triple }
    }
}

impl X86_64 {
    pub fn new(triple: Triple) -> X86_64 {
        X86_64 { triple: triple }
    }
}

impl LE32 {
    pub fn new(triple: Triple) -> LE32 {
        LE32 { triple: triple }
    }
}

impl Target for Arm {
    pub fn triple(&self) -> Triple {
        self.triple
    }

    pub fn data_layout<'r>(&'r self) -> &'r str {
        match self.triple.arch {
            Thumb(_) => {
                "e-p:32:32:32-i1:8:32-i8:8:32-i16:16:32-i32:32:32-\
                 i64:64:64-f32:32:32-f64:64:64-\
                 v64:64:64-v128:64:128-a0:0:32-n32-S64"
            }
            _ => {
                "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-\
                 i64:64:64-f32:32:32-f64:64:64-\
                 v64:64:64-v128:64:128-a0:0:64-n32-S64"
            }
        }
    }

    pub fn add_cc_args(&self, args: &mut ~[~str]) {
        args.push(~"-marm");
    }
}

impl Target for Mips {
    pub fn triple(&self) -> Triple {
        self.triple
    }

    pub fn data_layout<'r>(&'r self) -> &'r str {
        if self.triple.arch == Mips {
            "E-p:32:32:32-i1:8:8-i8:8:32-i16:16:32-i32:32:32-\
             i64:64:64-f32:32:32-f64:64:64-v64:64:64-n32-S64"
        } else {
            "e-p:32:32:32-i1:8:8-i8:8:32-i16:16:32-i32:32:32-\
             i64:64:64-f32:32:32-f64:64:64-v64:64:64-n32-S64"
        }
    }
}

impl Target for Mips64 {
    pub fn triple(&self) -> Triple {
        self.triple
    }

    pub fn data_layout<'r>(&'r self) -> &'r str {
        if self.triple.arch == Mips64 {
            "E-p:64:64:64-i1:8:8-i8:8:32-i16:16:32-i32:32:32-\
             i64:64:64-f32:32:32-f64:64:64-f128:128:128-\
             v64:64:64-n32:64-S128"
        } else {
            "e-p:64:64:64-i1:8:8-i8:8:32-i16:16:32-i32:32:32-\
             i64:64:64-f32:32:32-f64:64:64-f128:128:128-\
             v64:64:64-n32:64-S128"
        }
    }
}

impl Target for X86 {
    pub fn triple(&self) -> Triple {
        self.triple
    }

    pub fn data_layout<'r>(&'r self) -> &'r str {
        if self.triple.is_darwin() {
            "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-\
             i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-\
             a0:0:64-f80:128:128-n8:16:32-S128"
        } else if self.triple.os == Win32 || self.triple.os == MinGW32 {
            "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-\
             i64:64:64-f32:32:32-f64:64:64-f80:128:128-v64:64:64-\
             v128:128:128-a0:0:64-f80:32:32-n8:16:32-S32"
        } else if self.triple.os == Cygwin {
            "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-\
             i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-\
             a0:0:64-f80:32:32-n8:16:32-S32"
        } else {
            "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-\
             i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-\
             a0:0:64-f80:32:32-n8:16:32-S128"
        }
    }

    pub fn add_cc_args(&self, args: &mut ~[~str]) {
        args.push(~"-m32");
    }
}

impl Target for X86_64 {
    pub fn triple(&self) -> Triple {
        self.triple
    }

    pub fn data_layout<'r>(&'r self) -> &'r str {
        "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-\
         i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-\
         a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
    }

    pub fn add_cc_args(&self, args: &mut ~[~str]) {
        args.push(~"-m64");
    }
}

impl Target for LE32 {
    pub fn triple(&self) -> Triple {
        self.triple
    }

    pub fn data_layout<'r>(&'r self) -> &'r str {
        "e-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-\
         f32:32:32-f64:64:64-p:32:32:32-v128:32:32"
    }
}
