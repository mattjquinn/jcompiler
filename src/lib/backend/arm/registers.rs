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
pub enum ExtensionRegister {
    D0
}

impl std::fmt::Display for ExtensionRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            ExtensionRegister::D0 => f.write_str("d0")
        }
    }
}
