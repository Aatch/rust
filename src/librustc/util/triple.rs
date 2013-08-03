// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!

    Platform triple helper code based on similar code in LLVM.

    Parses strings representing platforms, historically called
    triples (despite being able to have 4 parts), of the form:

        ARCH-VENDOR-OS-ENVIRONMENT

    The main parser however will make a best-effort attempt at
    parsing any triple string.

    This also supports arbitrary versions on the the arm and
    thumb architechtures.
 */

use std;
use syntax::abi;
use metadata;

#[deriving(Eq)]
pub enum Arch {
    UnknownArch,
    Arm(u8), // With version suffix
    AArch64,
    Hexagon,
    Mips,
    MipsEL,
    Mips64,
    Mips64EL,
    MSP430,
    PPC,
    PPC64,
    PPC64LE,
    R600,
    Sparc,
    Sparcv9,
    SystemZ,
    TCE,
    Thumb(u8), // With version suffix
    X86,
    X86_64,
    XCore,
    NVPTX,
    NVPTX64,
    LE32,
    AmdIL,
    SPIR,
    SPIR64
}

#[deriving(Eq)]
pub enum Vendor {
    UnknownVendor,
    Apple,
    PC,
    SCEI,
    BGP,
    BGQ,
    Freescale,
    IBM,
    Nvidia
}

#[deriving(Eq)]
pub enum OSType {
    UnknownOS,
    AuroraUX,
    Cygwin,
    Darwin,
    DragonFly,
    FreeBSD,
    IOS,
    KFreeBSD,
    Linux,
    Lv2, // PS3
    MacOSX,
    MinGW32, // i*86-pc-mingw32, *-w64-mingw32
    NetBSD,
    OpenBSD,
    Solaris,
    Win32,
    Haiku,
    Minix,
    RTEMS,
    NaCl, // Native Client
    CNK, // BG/P Compute-Node Kernel
    Bitrig,
    AIX,
    CUDA, // Nvidia CUDA
    NVCL // Nvidia OpenCL
}

#[deriving(Eq)]
pub enum EnvironmentType {
    UnknownEnv,
    GNU,
    GNUEABI,
    GNUEABIHF,
    GNUX32,
    EABI,
    MachO,
    Android,
    ELF
}

#[deriving(Eq)]
pub struct Triple {
    arch: Arch,
    vendor: Vendor,
    os: OSType,
    env: EnvironmentType,
}

impl Arch {
    pub fn as_str(self) -> &'static str {
        match self {
            UnknownArch     => "unknown",
            Arm(_)          => "arm",
            AArch64         => "aarch64",
            Hexagon         => "hexagon",
            Mips            => "mips",
            MipsEL          => "miplel",
            Mips64          => "mips64",
            Mips64EL        => "mips64el",
            MSP430          => "msp430",
            PPC             => "ppc",
            PPC64           => "ppc64",
            PPC64LE         => "ppc64le",
            R600            => "r600",
            Sparc           => "sparc",
            Sparcv9         => "sparcv9",
            SystemZ         => "s390x",
            TCE             => "tce",
            Thumb(_)        => "thumb",
            X86             => "i386",
            X86_64          => "x86_64",
            XCore           => "xcore",
            NVPTX           => "nvptx",
            NVPTX64         => "nvptx64",
            LE32            => "le32",
            AmdIL           => "amdil",
            SPIR            => "spir",
            SPIR64          => "spir64"
        }
    }

    pub fn arch_prefix(self) -> Option<&'static str> {
        match self {
            AArch64 => Some("aarch64"),
            Arm(_) | Thumb(_) => Some("arm"),
            PPC | PPC64 | PPC64LE => Some("ppc"),
            Mips | Mips64 | MipsEL | Mips64EL => Some("mips"),
            Hexagon => Some("Hexagon"),
            R600 => Some("r600"),
            Sparc | Sparcv9 => Some("sparc"),
            SystemZ => Some("systemz"),
            X86 | X86_64 => Some("x86"),
            NVPTX | NVPTX64 => Some("nvptx"),
            LE32 => Some("le32"),
            AmdIL => Some("amdil"),
            SPIR | SPIR64 => Some("spir"),
            _ => None
        }
    }

    pub fn from_name(name: &str) -> Arch {
        match name {
            "i386" | "i486" | "i586"
          | "i686" | "i786" | "i868"
          | "i986" | "x86"                  => X86,

            "amd64" | "x86_64"              => X86_64,

            "powerpc" | "ppc" | "ppc32"     => PPC,
            "powerpc64" | "ppu" | "ppc64"   => PPC64,
            "powerpc64le" | "ppc64le"       => PPC64LE,
            "aarch64"                       => AArch64,
            "arm" | "xscale"                => Arm(0),
            "thumb"                         => Thumb(0),
            "msp430"                        => MSP430,
            "mips" | "mipseb" | "mipsallegrex" => Mips,
            "mipsel" | "mipsallegrexel"     => MipsEL,
            "mips64" | "mips64eb"           => Mips64,
            "mips64el"                      => Mips64EL,
            "r600"                          => R600,
            "hexagon"                       => Hexagon,
            "s390x"                         => SystemZ,
            "sparc"                         => Sparc,
            "sparcv9" | "sparc64"           => Sparcv9,
            "tce"                           => TCE,
            "xcore"                         => XCore,
            "nvptx"                         => NVPTX,
            "nvptx64"                       => NVPTX64,
            "le32"                          => LE32,
            "amdil"                         => AmdIL,
            "spir"                          => SPIR,
            "spir64"                        => SPIR64,
            arch if arch.starts_with("armv") => {
                let version = arch.slice_from(4);
                let vnum = std::u8::from_str(version);
                Arm(vnum.get_or_zero())
            }
            arch if arch.starts_with("thumbv") => {
                let version = arch.slice_from(6);
                let vnum = std::u8::from_str(version);
                Thumb(vnum.get_or_zero())
            }
            _                               => UnknownArch
        }
    }

    pub fn from_llvm_name(name: &str) -> Arch {
        match name {
            "aarch64"           => AArch64,
            "arm"               => Arm(0),
            "mips"              => Mips,
            "mipsel"            => MipsEL,
            "mips64"            => Mips64,
            "mips64el"          => Mips64EL,
            "msp430"            => MSP430,
            "ppc64"             => PPC64,
            "ppc32" | "ppc"     => PPC,
            "ppc64le"           => PPC64LE,
            "r600"              => R600,
            "hexagon"           => Hexagon,
            "sparc"             => Sparc,
            "sparcv9"           => Sparcv9,
            "systemz"           => SystemZ,
            "tce"               => TCE,
            "thumb"             => Thumb(0),
            "x86"               => X86,
            "x86-64"            => X86_64,
            "xcore"             => XCore,
            "nvptx"             => NVPTX,
            "nvptx64"           => NVPTX64,
            "le32"              => LE32,
            "amdil"             => AmdIL,
            "spir"              => SPIR,
            "spir64"            => SPIR64,
            _                   => UnknownArch
        }
    }

    /**
     * Size of pointer, in bits
     */
    pub fn pointer_size(self) -> uint {
        match self {
            UnknownArch     => 0,
            MSP430          => 16,

            AmdIL | Arm(_) | Hexagon
          | LE32 | Mips | MipsEL
          | NVPTX | PPC | R600
          | Sparc | TCE | Thumb(_)
          | X86 | XCore | SPIR => 32,

            AArch64 | Mips64 | Mips64EL
          | PPC64 | PPC64LE | Sparcv9
          | SystemZ | X86_64 | NVPTX64
          | SPIR64 => 64
        }
    }

    pub fn to_abi_arch(self) -> abi::Architecture {
        match self {
            X86 => abi::X86,
            X86_64 => abi::X86_64,
            Arm(_) => abi::Arm,
            Mips | MipsEL => abi::Mips,
            _ => fail!("Unsupported architures given")
        }
    }
}

impl Vendor {
    pub fn as_str(self) -> &'static str {
        match self {
            UnknownVendor   => "unknown",
            Apple           => "apple",
            PC              => "pc",
            SCEI            => "scei",
            BGP             => "bgp",
            BGQ             => "bgq",
            Freescale       => "freescale",
            IBM             => "ibm",
            Nvidia          => "nvidia"
        }
    }

    pub fn from_name(name: &str) -> Vendor {
        match name {
            "apple"         => Apple,
            "pc"            => PC,
            "scei"          => SCEI,
            "bgp"           => BGP,
            "bgq"           => BGQ,
            "fsl" | "freescale" => Freescale,
            "ibm"           => IBM,
            "nvidia"        => Nvidia,
            _               => UnknownVendor
        }
    }
}

macro_rules! str_starts(
    ($s:expr, { $($start:expr => $val:expr),+ } $default:expr) => (
        $(if ($s).starts_with($start) { $val })else+
        else { $default }
    )
)

impl OSType {
    pub fn as_str(self) -> &'static str {
        match self {
            UnknownOS       => "unknown",
            AuroraUX        => "auroraux",
            Cygwin          => "cygwin",
            Darwin          => "darwin",
            DragonFly       => "dragonfly",
            FreeBSD         => "freebsd",
            IOS             => "ios",
            KFreeBSD        => "kfreebsd",
            Linux           => "linux",
            Lv2             => "lv2",
            MacOSX          => "macosx",
            MinGW32        => "mingw32",
            NetBSD          => "netbsd",
            OpenBSD         => "openbsd",
            Solaris         => "solaris",
            Win32           => "win32",
            Haiku           => "haiku",
            Minix           => "minix",
            RTEMS           => "rtems",
            NaCl            => "nacl",
            CNK             => "cnk",
            Bitrig          => "bitrig",
            AIX             => "aix",
            CUDA            => "cuda",
            NVCL            => "nvcl"
        }
    }

    pub fn from_name(name: &str) -> OSType {
        str_starts!(name, {
            "auroraux"      => AuroraUX,
            "cygwin"        => Cygwin,
            "darwin"        => Darwin,
            "dragonfly"     => DragonFly,
            "freebsd"       => FreeBSD,
            "ios"           => IOS,
            "kfreebsd"      => KFreeBSD,
            "linux"         => Linux,
            "lv2"           => Lv2,
            "macosx"        => MacOSX,
            "mingw32"       => MinGW32,
            "netbsd"        => NetBSD,
            "openbsd"       => OpenBSD,
            "solaris"       => Solaris,
            "win32"         => Win32,
            "haiku"         => Haiku,
            "minix"         => Minix,
            "rtems"         => RTEMS,
            "nacl"          => NaCl,
            "cnk"           => CNK,
            "bitrig"        => Bitrig,
            "aix"           => AIX,
            "cuda"          => CUDA,
            "nvcl"          => NVCL
        } UnknownOS)
    }
}

impl EnvironmentType {
    pub fn as_str(self) -> &'static str {
        match self {
            UnknownEnv      => "unknown",
            GNU             => "gnu",
            GNUEABI         => "gnueabi",
            GNUEABIHF       => "gnueabihf",
            GNUX32          => "gnux32",
            EABI            => "eabi",
            MachO           => "macho",
            Android         => "android",
            ELF             => "elf"
        }
    }

    pub fn from_name(name: &str) -> EnvironmentType {
        str_starts!(name, {
            "eabi"          => EABI,
            "gnueabihf"     => GNUEABIHF,
            "gnueabi"       => GNUEABI,
            "gnux32"        => GNUX32,
            "gnu"           => GNU,
            "macho"         => MachO,
            "android"       => Android,
            "elf"           => ELF
        } UnknownEnv)
    }
}

impl Triple {

    pub fn new() -> Triple {
        Triple {
            arch: UnknownArch,
            vendor: UnknownVendor,
            os: UnknownOS,
            env: UnknownEnv,
        }
    }

    /**
     * Parse a triple string. Makes a best-effort attempt to deal with
     * things like missing parts (i386-linux-gnu) and re-ordered parts
     * (i386-linux-unknown-gnu).
     */
    pub fn from_triple(triple: &str) -> Triple {

        let mut split = triple.split_iter('-');
        let parts = [
            split.next().get_or_default(""),
            split.next().get_or_default(""),
            split.next().get_or_default(""),
            split.next().get_or_default("")
        ];

        // Try to parse the elements into their expected positions
        let mut arch    = Arch::from_name(parts[0]);
        let mut vendor  = Vendor::from_name(parts[1]);
        let mut os      = OSType::from_name(parts[2]);
        let mut env     = EnvironmentType::from_name(parts[3]);

        // Track which have been matched already
        let mut matched = [
            arch != UnknownArch,
            vendor != UnknownVendor,
            os != UnknownOS,
            env != UnknownEnv
        ];

        foreach (i, _) in parts.iter().enumerate() {
            foreach (j, &try_match) in parts.iter().enumerate() {
                if !matched[j] {
                    if i == 0 && arch == UnknownArch {
                        arch = Arch::from_name(try_match);
                        matched[j] = arch != UnknownArch;
                    } else if i == 1 && vendor == UnknownVendor {
                        vendor = Vendor::from_name(try_match);
                        matched[j] = vendor != UnknownVendor;
                    } else if i == 2 && os == UnknownOS {
                        os = OSType::from_name(try_match);
                        matched[j] = os != UnknownOS;
                    } else if i == 3 && env == UnknownEnv {
                        env = EnvironmentType::from_name(try_match);
                        matched[j] = env != UnknownEnv;
                    }
                    if matched[j] { break; }
                }
            }
        }

        Triple {
            arch: arch,
            vendor: vendor,
            os: os,
            env: env
        }
    }


    pub fn meta_os(&self) -> metadata::loader::os {
        use metadata::loader;

        if self.env == Android {
            loader::os_android
        } else if self.is_windows() {
            loader::os_win32
        } else if self.is_macosx() {
            loader::os_macos
        } else if self.os == FreeBSD || self.os == KFreeBSD {
            loader::os_freebsd
        } else if self.os == Linux {
            loader::os_linux
        } else {
            fail!("Unsupported operating system %s", self.os.as_str())
        }
    }

    // Predicates

    #[inline]
    pub fn has_environment(&self) -> bool {
        self.env != UnknownEnv
    }

    pub fn is_arch_64bit(&self) -> bool {
        self.arch.pointer_size() == 64
    }

    pub fn is_arch_32bit(&self) -> bool {
        self.arch.pointer_size() == 32
    }

    pub fn is_arch_16bit(&self) -> bool {
        self.arch.pointer_size() == 16
    }

    #[inline]
    pub fn is_macosx(&self) -> bool {
        self.os == Darwin || self.os == MacOSX
    }

    #[inline]
    pub fn is_ios(&self) -> bool {
        self.os == IOS
    }

    #[inline]
    pub fn is_darwin(&self) -> bool {
        self.is_macosx() || self.is_ios()
    }

    #[inline]
    pub fn is_cyg_ming(&self) -> bool {
        self.os == Cygwin || self.os == MinGW32
    }

    #[inline]
    pub fn is_windows(&self) -> bool {
        self.os == Win32 || self.is_cyg_ming()
    }

    #[inline]
    pub fn is_binfmt_elf(&self) -> bool {
        !self.is_darwin() && !self.is_windows()
    }

    #[inline]
    pub fn is_binfmt_coff(&self) -> bool {
        self.is_windows()
    }

    #[inline]
    pub fn is_binfmt_macho(&self) -> bool {
        self.env == MachO || self.is_darwin()
    }

    pub fn lib_prefix(&self) -> &'static str {
        if self.is_binfmt_coff() {
            ""
        } else if self.is_binfmt_elf() {
            "lib"
        } else if self.is_binfmt_macho() {
            "lib"
        } else { "" }
    }

    pub fn lib_suffix(&self) -> &'static str {
        if self.is_binfmt_coff() {
            ".dll"
        } else if self.is_binfmt_elf() {
            ".so"
        } else if self.is_binfmt_macho() {
            ".dylib"
        } else { "" }
    }

    pub fn exe_suffix(&self) -> &'static str {
        if self.is_binfmt_coff() {
            ".exe"
        } else if self.is_binfmt_elf() {
            ""
        } else if self.is_binfmt_macho() {
            ""
        } else { "" }
    }
}

impl ToStr for Triple {
    pub fn to_str(&self) -> ~str {
        fmt!("%s-%s-%s-%s",
            self.arch.as_str(),
            self.vendor.as_str(),
            self.os.as_str(),
            self.env.as_str())
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn parse_simple() {
        let triple = Triple::from_triple("x86_64-unknown-linux-gnu");

        assert_eq!(triple, Triple {
            arch: X86_64,
            vendor: UnknownVendor,
            os: Linux,
            env: GNU
        });
    }

    #[test]
    fn parse_missing_env() {
        let triple = Triple::from_triple("x86_64-unknown-linux");

        assert_eq!(triple, Triple {
            arch: X86_64,
            vendor: UnknownVendor,
            os: Linux,
            env: UnknownEnv
        });
    }

    #[test]
    fn parse_wrong_order() {
        let triple = Triple::from_triple("x86_64-gnu-linux");

        assert_eq!(triple, Triple {
            arch: X86_64,
            vendor: UnknownVendor,
            os: Linux,
            env: GNU
        });
    }

    #[test]
    fn parse_reversed() {
        let triple = Triple::from_triple("gnu-linux-x86");

        assert_eq!(triple, Triple {
            arch: X86,
            vendor: UnknownVendor,
            os: Linux,
            env: GNU
        });
    }

    #[test]
    fn reparse() {
        let triple = Triple {
            arch: X86,
            vendor: Nvidia,
            os: Linux,
            env: Android
        };

        let triple_str = triple.to_str();
        let reparsed = Triple::from_triple(triple_str);

        assert_eq!(triple, reparsed);
    }

    #[test]
    fn arm_version() {
        let triple = Triple::from_triple("armv5-unknown-linux-eabi");
        assert_eq!(triple, Triple {
            arch: Arm(5),
            vendor: UnknownVendor,
            os: Linux,
            env: EABI
        });
    }
}
