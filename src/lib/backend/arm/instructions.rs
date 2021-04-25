use itertools::free::join;
use backend::arm::registers::ArmRegister;

#[derive(Debug)]
pub enum ArmIns {
    Load { dst: String, src: String },
    Store { dst: String, src: String },
    LoadOffset { dst: &'static str, src: &'static str, offsets: Vec<i32> },
    StoreOffsetDeprecated { dst: &'static str, src: &'static str, offsets: Vec<i32> },
    BranchAndLink { addr: &'static str },
    Add { dst: &'static str, src: &'static str, add: &'static str },
    AddImmDeprecated { dst: &'static str, src: &'static str, imm: i32 },
    Sub { dst: &'static str, src: &'static str, sub: &'static str },
    SubImm { dst: &'static str, src: &'static str, imm: i32 },
    Move { dst: &'static str, src: &'static str },
    MoveImmDeprecated { dst: &'static str, imm: i32 },
    Multiply { dst: &'static str, src: &'static str, mul: &'static str },
    Compare { lhs: &'static str, rhs: &'static str },
    MoveLT { dst: &'static str, src: &'static str },
    MoveLE { dst: &'static str, src: &'static str },
    MoveGT { dst: &'static str, src: &'static str },
    MoveGE { dst: &'static str, src: &'static str },
    MoveNE { dst: &'static str, src: &'static str },
    MoveEQ { dst: &'static str, src: &'static str },
    Nop,

    MoveImm { dst: ArmRegister, imm: i32 },
    StoreOffset { dst: ArmRegister, src: ArmRegister, offsets: Vec<i32> },
    AddImm { dst: ArmRegister, src: ArmRegister, imm: i32 },
}

impl std::fmt::Display for ArmIns {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match self {
            ArmIns::Load { dst, src } =>
                f.write_str(format!("ldr {}, {}", dst, src).as_str()),
            ArmIns::Store { dst, src } =>
                f.write_str(format!("str {}, [{}]", src, dst).as_str()),
            ArmIns::LoadOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("ldr {}, [{}, {}]", dst, src, offset_str).as_str())
            }
            ArmIns::StoreOffsetDeprecated { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("str {}, [{}, {}]", src, dst, offset_str).as_str())
            }
            ArmIns::Compare { lhs, rhs } => {
                f.write_str(format!("cmp {}, {}", lhs, rhs).as_str())
            }
            ArmIns::MoveLT { dst, src } => {
                f.write_str(format!("movlt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveLE { dst, src } => {
                f.write_str(format!("movle {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGT { dst, src } => {
                f.write_str(format!("movgt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGE { dst, src } => {
                f.write_str(format!("movge {}, {}", dst, src).as_str())
            }
            ArmIns::MoveEQ { dst, src } => {
                f.write_str(format!("moveq {}, {}", dst, src).as_str())
            }
            ArmIns::MoveNE { dst, src } => {
                f.write_str(format!("movne {}, {}", dst, src).as_str())
            }
            ArmIns::BranchAndLink { addr } => f.write_str(format!("bl {}", addr).as_str()),
            ArmIns::Move { dst, src } =>
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
            ArmIns::SubImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("sub {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("sub {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::Sub { dst, src, sub } => {
                f.write_str(format!("sub {}, {}, {}", dst, src, sub).as_str())
            }
            ArmIns::Add { dst, src, add } => {
                f.write_str(format!("add {}, {}, {}", dst, src, add).as_str())
            }
            ArmIns::Multiply { dst, src, mul } => {
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
