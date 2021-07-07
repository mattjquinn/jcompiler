use itertools::free::join;
use backend::arm::registers::{CoreRegister, ExtensionRegister};

#[derive(Debug)]
pub enum ArmIns {
    Move { dst: CoreRegister, src: CoreRegister },
    MoveImm { dst: CoreRegister, imm: i32 }, // TODO: should imm be narrowed in accordance with ARM's actual allowed width?
    MoveImmUnsigned { dst: CoreRegister, imm: u16 }, // TODO: is this width for imm true to actual ARM width?
    StoreOffset { dst: CoreRegister, src: CoreRegister, offsets: Vec<i32> },
    Store { dst: CoreRegister, src: CoreRegister },
    AddImm { dst: CoreRegister, src: CoreRegister, imm: i32 },
    SubImm { dst: CoreRegister, src: CoreRegister, imm: i32 },
    Load { dst: CoreRegister, src: String },
    LoadOffset { dst: CoreRegister, src: CoreRegister, offsets: Vec<i32> },
    Multiply { dst: CoreRegister, src: CoreRegister, mul: CoreRegister },
    Sub { dst: CoreRegister, src: CoreRegister, sub: CoreRegister },
    Add { dst: CoreRegister, src: CoreRegister, add: CoreRegister },
    Compare { lhs: CoreRegister, rhs: CoreRegister },
    MoveLT { dst: CoreRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveGE { dst: CoreRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveEQ { dst: CoreRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveNE { dst: CoreRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveGT { dst: CoreRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    MoveLE { dst: CoreRegister, src: i8 },  // can probably be wider than i8, need to check highest acceptable width
    LeftShift { dst: CoreRegister, src: CoreRegister, n_bits: i8 },
    BranchAndLink { addr: &'static str },
    ExclusiveOr { dst: CoreRegister, src: CoreRegister, operand: u32 },
    LoadExtensionRegisterWidth64 { dst: ExtensionRegister, src: CoreRegister, offsets: Vec<i32> }
}

impl std::fmt::Display for ArmIns {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match self {
            ArmIns::BranchAndLink { addr } => f.write_str(format!("bl {}", addr).as_str()),
            ArmIns::Move { dst, src } =>
                f.write_str(format!("mov {}, {}", dst, src).as_str()),
            ArmIns::MoveImm { dst, imm } => f.write_str(format!("mov {}, #{}", dst, imm).as_str()),
            ArmIns::MoveImmUnsigned { dst, imm } => {
                f.write_str(format!("mov {}, #0x{}", dst, format!("{:04x}", imm)).as_str())
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
                f.write_str(format!("lsl {}, {}, #{}", dst, src, n_bits).as_str())
            }
            ArmIns::SubImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("sub {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("sub {}, {}, {}", dst, src, imm).as_str())
                }
            },
            ArmIns::ExclusiveOr { dst, src, operand } => {
                f.write_str(format!("eor {}, {}, #0x{}", dst, src, format!("{:08x}", operand)).as_str())
            }
            ArmIns::LoadExtensionRegisterWidth64 { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("vldr.64 {}, [{}, {}]", dst, src, offset_str).as_str())
            }
        }
    }
}
