use itertools::free::join;
use backend::arm::registers::ArmRegister;

#[derive(Debug)]
pub enum ArmIns {
    Move { dst: ArmRegister, src: ArmRegister },
    MoveImm { dst: ArmRegister, imm: i32 }, // TODO: should imm be narrowed in accordance with ARM's actual allowed width?
    MoveImmUnsigned { dst: ArmRegister, imm: u16 }, // TODO: is this width for imm true to actual ARM width?
    StoreOffset { dst: ArmRegister, src: ArmRegister, offsets: Vec<i32> },
    Store { dst: ArmRegister, src: ArmRegister },
    AddImm { dst: ArmRegister, src: ArmRegister, imm: i32 },
    SubImm { dst: ArmRegister, src: ArmRegister, imm: i32 },
    Load { dst: ArmRegister, src: String },
    LoadOffset { dst: ArmRegister, src: ArmRegister, offsets: Vec<i32> },
    Multiply { dst: ArmRegister, src: ArmRegister, mul: ArmRegister },
    Sub { dst: ArmRegister, src: ArmRegister, sub: ArmRegister },
    Add { dst: ArmRegister, src: ArmRegister, add: ArmRegister },
    Compare { lhs: ArmRegister, rhs: ArmRegister },
    MoveLT { dst: ArmRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveGE { dst: ArmRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveEQ { dst: ArmRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveNE { dst: ArmRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveGT { dst: ArmRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveLE { dst: ArmRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    LeftShift { dst: ArmRegister, src: ArmRegister, n_bits: i8 },
    BranchAndLink { addr: &'static str },
}

impl std::fmt::Display for ArmIns {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match self {
            ArmIns::BranchAndLink { addr } => f.write_str(format!("bl {}", addr).as_str()),
            ArmIns::Move { dst, src } =>
                f.write_str(format!("mov {}, {}", dst, src).as_str()),
            ArmIns::MoveImm { dst, imm } => f.write_str(format!("mov {}, {}", dst, imm).as_str()),
            ArmIns::MoveImmUnsigned { dst, imm } => {
                f.write_str(format!("mov {}, #0x{}", dst, format!("{:02x}", imm)).as_str())
            },
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
            ArmIns::LoadOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("ldr {}, [{}, {}]", dst, src, offset_str).as_str())
            }
            ArmIns::Multiply { dst, src, mul } => {
                f.write_str(format!("mul {}, {}, {}", dst, src, mul).as_str())
            }
            ArmIns::Sub { dst, src, sub } => {
                f.write_str(format!("sub {}, {}, {}", dst, src, sub).as_str())
            }
            ArmIns::Load { dst, src } => {
                f.write_str(format!("ldr {}, {}", dst, src).as_str())
            }
            ArmIns::Add { dst, src, add } => {
                f.write_str(format!("add {}, {}, {}", dst, src, add).as_str())
            }
            ArmIns::Compare { lhs, rhs } => {
                f.write_str(format!("cmp {}, {}", lhs, rhs).as_str())
            }
            ArmIns::MoveLT { dst, src } => {
                f.write_str(format!("movlt {}, #{}", dst, src).as_str())
            }
            ArmIns::MoveGE { dst, src } => {
                f.write_str(format!("movge {}, #{}", dst, src).as_str())
            }
            ArmIns::MoveEQ { dst, src } => {
                f.write_str(format!("moveq {}, #{}", dst, src).as_str())
            }
            ArmIns::MoveNE { dst, src } => {
                f.write_str(format!("movne {}, #{}", dst, src).as_str())
            }
            ArmIns::MoveGT { dst, src } => {
                f.write_str(format!("movgt {}, #{}", dst, src).as_str())
            }
            ArmIns::MoveLE { dst, src } => {
                f.write_str(format!("movle {}, #{}", dst, src).as_str())
            }
            ArmIns::Store { dst, src } => {
                f.write_str(format!("str {}, [{}]", src, dst).as_str())
            }
            ArmIns::LeftShift { dst, src, n_bits} => {
                f.write_str(format!("lsl {}, {}, #{}", src, dst, n_bits).as_str())
            }
            ArmIns::SubImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("sub {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("sub {}, {}, {}", dst, src, imm).as_str())
                }
            }
        }
    }
}
