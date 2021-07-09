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
            ArmIns::BranchAndLink { addr } =>
                f.write_str(format!("\tbl\t{}", addr).as_str()),
            ArmIns::Move { dst, src } =>
                f.write_str(format!("\tmov\t{}, {}", dst, src).as_str()),
            ArmIns::MoveImm { dst, imm } =>
                f.write_str(format!("\tmov\t{}, #{}", dst, imm).as_str()),
            ArmIns::MoveImmUnsigned { dst, imm } => {
                f.write_str(format!("\tmov\t{}, #0x{}", dst, format!("{:04x}", imm)).as_str())
            },
            ArmIns::StoreOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("\tstr\t{}, [{}, {}]", src, dst, offset_str).as_str())
            }
            ArmIns::AddImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("\tadd\t{}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("\tadd\t{}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::LoadOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("\tldr\t{}, [{}, {}]", dst, src, offset_str).as_str())
            }
            ArmIns::Multiply { dst, src, mul } => {
                f.write_str(format!("\tmul\t{}, {}, {}", dst, src, mul).as_str())
            }
            ArmIns::Sub { dst, src, sub } => {
                f.write_str(format!("\tsub\t{}, {}, {}", dst, src, sub).as_str())
            }
            ArmIns::Load { dst, src } => {
                f.write_str(format!("\tldr\t{}, {}", dst, src).as_str())
            }
            ArmIns::Add { dst, src, add } => {
                f.write_str(format!("\tadd\t{}, {}, {}", dst, src, add).as_str())
            }
            ArmIns::Compare { lhs, rhs } => {
                f.write_str(format!("\tcmp\t{}, {}", lhs, rhs).as_str())
            }
            ArmIns::MoveLT { dst, src } => {
                f.write_str(format!("\tmovlt\t{}, #{}", dst, src).as_str())
            }
            ArmIns::MoveGE { dst, src } => {
                f.write_str(format!("\tmovge\t{}, #{}", dst, src).as_str())
            }
            ArmIns::MoveEQ { dst, src } => {
                f.write_str(format!("\tmoveq\t{}, #{}", dst, src).as_str())
            }
            ArmIns::MoveNE { dst, src } => {
                f.write_str(format!("\tmovne\t{}, #{}", dst, src).as_str())
            }
            ArmIns::MoveGT { dst, src } => {
                f.write_str(format!("\tmovgt\t{}, #{}", dst, src).as_str())
            }
            ArmIns::MoveLE { dst, src } => {
                f.write_str(format!("\tmovle\t{}, #{}", dst, src).as_str())
            }
            ArmIns::Store { dst, src } => {
                f.write_str(format!("\tstr\t{}, [{}]", src, dst).as_str())
            }
            ArmIns::LeftShift { dst, src, n_bits} => {
                f.write_str(format!("\tlsl\t{}, {}, #{}", dst, src, n_bits).as_str())
            }
            ArmIns::SubImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("\tsub\t{}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("\tsub\t{}, {}, {}", dst, src, imm).as_str())
                }
            },
            ArmIns::ExclusiveOr { dst, src, operand } => {
                f.write_str(format!("\teor\t{}, {}, #0x{}", dst, src, format!("{:08x}", operand)).as_str())
            }
            ArmIns::LoadExtensionRegisterWidth64 { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("\tvldr.64\t{}, [{}, {}]", dst, src, offset_str).as_str())
            }
        }
    }
}
