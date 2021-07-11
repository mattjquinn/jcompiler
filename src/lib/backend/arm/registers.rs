use std::fmt::{Formatter, Error};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum CoreRegister {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    FP,
    IP,
    SP,
    LR,
    PC,
    CPSR
}

impl std::fmt::Display for CoreRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            CoreRegister::R0 => f.write_str("r0"),
            CoreRegister::R1 => f.write_str("r1"),
            CoreRegister::R2 => f.write_str("r2"),
            CoreRegister::R3 => f.write_str("r3"),
            CoreRegister::R4 => f.write_str("r4"),
            CoreRegister::R5 => f.write_str("r5"),
            CoreRegister::R6 => f.write_str("r6"),
            CoreRegister::R7 => f.write_str("r7"),
            CoreRegister::R8 => f.write_str("r8"),
            CoreRegister::R9 => f.write_str("r9"),
            CoreRegister::R10 => f.write_str("r10"),
            CoreRegister::FP => f.write_str("fp"),
            CoreRegister::IP => f.write_str("ip"),
            CoreRegister::SP => f.write_str("sp"),
            CoreRegister::LR => f.write_str("lr"),
            CoreRegister::PC => f.write_str("rc"),
            CoreRegister::CPSR => f.write_str("cpsr"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum ExtensionRegisterSinglePrecision {
    // Per https://developer.arm.com/documentation/dui0473/c/CJAIJHFC
    // these overlap with the other n-precision extension registers,
    // so overlapped use of them will cause bugs.
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    S12,
    S13,
    S14,
    S15,
    S16,
    S17,
    S18,
    S19,
    S20,
    S21,
    S22,
    S23,
    S24,
    S25,
    S26,
    S27,
    S28,
    S29,
    S30,
    S31
}

impl std::fmt::Display for ExtensionRegisterSinglePrecision {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            ExtensionRegisterSinglePrecision::S0 => f.write_str("s0"),
            ExtensionRegisterSinglePrecision::S1 => f.write_str("s1"),
            ExtensionRegisterSinglePrecision::S2 => f.write_str("s2"),
            ExtensionRegisterSinglePrecision::S3 => f.write_str("s3"),
            ExtensionRegisterSinglePrecision::S4 => f.write_str("s4"),
            ExtensionRegisterSinglePrecision::S5 => f.write_str("s5"),
            ExtensionRegisterSinglePrecision::S6 => f.write_str("s6"),
            ExtensionRegisterSinglePrecision::S7 => f.write_str("s7"),
            ExtensionRegisterSinglePrecision::S8 => f.write_str("s8"),
            ExtensionRegisterSinglePrecision::S9 => f.write_str("s9"),
            ExtensionRegisterSinglePrecision::S10 => f.write_str("s10"),
            ExtensionRegisterSinglePrecision::S11 => f.write_str("s11"),
            ExtensionRegisterSinglePrecision::S12 => f.write_str("s12"),
            ExtensionRegisterSinglePrecision::S13 => f.write_str("s13"),
            ExtensionRegisterSinglePrecision::S14 => f.write_str("s14"),
            ExtensionRegisterSinglePrecision::S15 => f.write_str("s15"),
            ExtensionRegisterSinglePrecision::S16 => f.write_str("s16"),
            ExtensionRegisterSinglePrecision::S17 => f.write_str("s17"),
            ExtensionRegisterSinglePrecision::S18 => f.write_str("s18"),
            ExtensionRegisterSinglePrecision::S19 => f.write_str("s19"),
            ExtensionRegisterSinglePrecision::S20 => f.write_str("s20"),
            ExtensionRegisterSinglePrecision::S21 => f.write_str("s21"),
            ExtensionRegisterSinglePrecision::S22 => f.write_str("s22"),
            ExtensionRegisterSinglePrecision::S23 => f.write_str("s23"),
            ExtensionRegisterSinglePrecision::S24 => f.write_str("s24"),
            ExtensionRegisterSinglePrecision::S25 => f.write_str("s25"),
            ExtensionRegisterSinglePrecision::S26 => f.write_str("s26"),
            ExtensionRegisterSinglePrecision::S27 => f.write_str("s27"),
            ExtensionRegisterSinglePrecision::S28 => f.write_str("s28"),
            ExtensionRegisterSinglePrecision::S29 => f.write_str("s29"),
            ExtensionRegisterSinglePrecision::S30 => f.write_str("s30"),
            ExtensionRegisterSinglePrecision::S31 => f.write_str("s31")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum ExtensionRegisterDoublePrecision {
    // Per https://developer.arm.com/documentation/dui0473/c/CJAIJHFC
    // these overlap with the other n-precision extension registers,
    // so overlapped use of them will cause bugs.
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    D9,
    D10,
    D11,
    D12,
    D13,
    D14,
    D15,
    D16,
    D17,
    D18,
    D19,
    D20,
    D21,
    D22,
    D23,
    D24,
    D25,
    D26,
    D27,
    D28,
    D29,
    D30,
    D31
}

impl std::fmt::Display for ExtensionRegisterDoublePrecision {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            ExtensionRegisterDoublePrecision::D0 => f.write_str("d0"),
            ExtensionRegisterDoublePrecision::D1 => f.write_str("d1"),
            ExtensionRegisterDoublePrecision::D2 => f.write_str("d2"),
            ExtensionRegisterDoublePrecision::D3 => f.write_str("d3"),
            ExtensionRegisterDoublePrecision::D4 => f.write_str("d4"),
            ExtensionRegisterDoublePrecision::D5 => f.write_str("d5"),
            ExtensionRegisterDoublePrecision::D6 => f.write_str("d6"),
            ExtensionRegisterDoublePrecision::D7 => f.write_str("d7"),
            ExtensionRegisterDoublePrecision::D8 => f.write_str("d8"),
            ExtensionRegisterDoublePrecision::D9 => f.write_str("d9"),
            ExtensionRegisterDoublePrecision::D10 => f.write_str("d10"),
            ExtensionRegisterDoublePrecision::D11 => f.write_str("d11"),
            ExtensionRegisterDoublePrecision::D12 => f.write_str("d12"),
            ExtensionRegisterDoublePrecision::D13 => f.write_str("d13"),
            ExtensionRegisterDoublePrecision::D14 => f.write_str("d14"),
            ExtensionRegisterDoublePrecision::D15 => f.write_str("d15"),
            ExtensionRegisterDoublePrecision::D16 => f.write_str("d16"),
            ExtensionRegisterDoublePrecision::D17 => f.write_str("d17"),
            ExtensionRegisterDoublePrecision::D18 => f.write_str("d18"),
            ExtensionRegisterDoublePrecision::D19 => f.write_str("d19"),
            ExtensionRegisterDoublePrecision::D20 => f.write_str("d20"),
            ExtensionRegisterDoublePrecision::D21 => f.write_str("d21"),
            ExtensionRegisterDoublePrecision::D22 => f.write_str("d22"),
            ExtensionRegisterDoublePrecision::D23 => f.write_str("d23"),
            ExtensionRegisterDoublePrecision::D24 => f.write_str("d24"),
            ExtensionRegisterDoublePrecision::D25 => f.write_str("d25"),
            ExtensionRegisterDoublePrecision::D26 => f.write_str("d26"),
            ExtensionRegisterDoublePrecision::D27 => f.write_str("d27"),
            ExtensionRegisterDoublePrecision::D28 => f.write_str("d28"),
            ExtensionRegisterDoublePrecision::D29 => f.write_str("d29"),
            ExtensionRegisterDoublePrecision::D30 => f.write_str("d30"),
            ExtensionRegisterDoublePrecision::D31 => f.write_str("d31")
        }
    }
}
