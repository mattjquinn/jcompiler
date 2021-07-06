use std::fmt::{Formatter, Error};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum ArmRegister {
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

impl std::fmt::Display for ArmRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            ArmRegister::R0 => f.write_str("r0"),
            ArmRegister::R1 => f.write_str("r1"),
            ArmRegister::R2 => f.write_str("r2"),
            ArmRegister::R3 => f.write_str("r3"),
            ArmRegister::R4 => f.write_str("r4"),
            ArmRegister::R5 => f.write_str("r5"),
            ArmRegister::R6 => f.write_str("r6"),
            ArmRegister::R7 => f.write_str("r7"),
            ArmRegister::R8 => f.write_str("r8"),
            ArmRegister::R9 => f.write_str("r9"),
            ArmRegister::R10 => f.write_str("r10"),
            ArmRegister::FP => f.write_str("fp"),
            ArmRegister::IP => f.write_str("ip"),
            ArmRegister::SP => f.write_str("sp"),
            ArmRegister::LR => f.write_str("lr"),
            ArmRegister::PC => f.write_str("rc"),
            ArmRegister::CPSR => f.write_str("cpsr"),
        }
    }
}
