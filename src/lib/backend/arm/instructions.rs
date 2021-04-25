use itertools::free::join;
use backend::arm::registers::ArmRegister;

#[derive(Debug)]
pub enum ArmIns {
    LoadDeprecated { dst: String, src: String },
    StoreDeprecated { dst: String, src: String },
    LoadOffsetDeprecated { dst: &'static str, src: &'static str, offsets: Vec<i32> },
    StoreOffsetDeprecated { dst: &'static str, src: &'static str, offsets: Vec<i32> },
    BranchAndLinkDeprecated { addr: &'static str },
    AddDeprecated { dst: &'static str, src: &'static str, add: &'static str },
    AddImmDeprecated { dst: &'static str, src: &'static str, imm: i32 },
    SubDeprecated { dst: &'static str, src: &'static str, sub: &'static str },
    SubImmDeprecated { dst: &'static str, src: &'static str, imm: i32 },
    MoveDeprecated { dst: &'static str, src: &'static str },
    MoveImmDeprecated { dst: &'static str, imm: i32 },
    MultiplyDeprecated { dst: &'static str, src: &'static str, mul: &'static str },
    CompareDeprecated { lhs: &'static str, rhs: &'static str },
    MoveLTDeprecated { dst: &'static str, src: &'static str },
    MoveLEDeprecated { dst: &'static str, src: &'static str },
    MoveGTDeprecated { dst: &'static str, src: &'static str },
    MoveGEDeprecated { dst: &'static str, src: &'static str },
    MoveNEDeprecated { dst: &'static str, src: &'static str },
    MoveEQDeprecated { dst: &'static str, src: &'static str },

    Nop,
    MoveImm { dst: ArmRegister, imm: i32 },
    StoreOffset { dst: ArmRegister, src: ArmRegister, offsets: Vec<i32> },
    AddImm { dst: ArmRegister, src: ArmRegister, imm: i32 },
}

impl std::fmt::Display for ArmIns {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match self {
            ArmIns::LoadDeprecated { dst, src } =>
                f.write_str(format!("ldr {}, {}", dst, src).as_str()),
            ArmIns::StoreDeprecated { dst, src } =>
                f.write_str(format!("str {}, [{}]", src, dst).as_str()),
            ArmIns::LoadOffsetDeprecated { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("ldr {}, [{}, {}]", dst, src, offset_str).as_str())
            }
            ArmIns::StoreOffsetDeprecated { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("str {}, [{}, {}]", src, dst, offset_str).as_str())
            }
            ArmIns::CompareDeprecated { lhs, rhs } => {
                f.write_str(format!("cmp {}, {}", lhs, rhs).as_str())
            }
            ArmIns::MoveLTDeprecated { dst, src } => {
                f.write_str(format!("movlt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveLEDeprecated { dst, src } => {
                f.write_str(format!("movle {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGTDeprecated { dst, src } => {
                f.write_str(format!("movgt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGEDeprecated { dst, src } => {
                f.write_str(format!("movge {}, {}", dst, src).as_str())
            }
            ArmIns::MoveEQDeprecated { dst, src } => {
                f.write_str(format!("moveq {}, {}", dst, src).as_str())
            }
            ArmIns::MoveNEDeprecated { dst, src } => {
                f.write_str(format!("movne {}, {}", dst, src).as_str())
            }
            ArmIns::BranchAndLinkDeprecated { addr } => f.write_str(format!("bl {}", addr).as_str()),
            ArmIns::MoveDeprecated { dst, src } =>
                f.write_str(format!("mov {}, {}", dst, src).as_str()),
            ArmIns::MoveImmDeprecated { dst, imm } => f.write_str(format!("mov {}, {}", dst, imm).as_str()),
            ArmIns::AddImmDeprecated { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("add {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("add {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::SubImmDeprecated { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("sub {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("sub {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::SubDeprecated { dst, src, sub } => {
                f.write_str(format!("sub {}, {}, {}", dst, src, sub).as_str())
            }
            ArmIns::AddDeprecated { dst, src, add } => {
                f.write_str(format!("add {}, {}, {}", dst, src, add).as_str())
            }
            ArmIns::MultiplyDeprecated { dst, src, mul } => {
                f.write_str(format!("mul {}, {}, {}", dst, src, mul).as_str())
            }
            ArmIns::Nop => f.write_str("nop"),




            ArmIns::MoveImm { dst, imm } => f.write_str(format!("mov {}, {}", dst, imm).as_str()),
            ArmIns::StoreOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("str {}, [{}, {}]", src, dst, offset_str).as_str())
            }
            ArmIns::AddImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("add {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("add {}, {}, {}", dst, src, imm).as_str())
                }
            }
        }
    }
}
