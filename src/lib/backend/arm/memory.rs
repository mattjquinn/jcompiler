use backend::arm::registers::{CoreRegister};
use super::instructions::{ArmIns};
use super::compiler::{BasicBlock};
use std::fmt::{Formatter, Error};

#[derive(Debug)]
pub enum Pointer {
    Stack(i32),  // an offset from sp into the line-extent stack
    Heap(i32),  // an offset from fp into the process-extent heap
}

impl Pointer {

    fn get_offset(&self) -> (CoreRegister, i32) {
        match self {
            Pointer::Stack(offset) => (CoreRegister::SP, *offset),
            Pointer::Heap(offset) => (CoreRegister::FP, *offset)
        }
    }

    pub fn read(&self, dst: CoreRegister, basic_block: &mut BasicBlock) {
        let (src, offset) = self.get_offset();
        basic_block.push(ArmIns::LoadOffset {
            dst,
            src,
            offsets: vec![offset]
        });
    }

    pub fn write(&self, src: CoreRegister, basic_block: &mut BasicBlock) {
        let (dst, offset) = self.get_offset();
        basic_block.push(ArmIns::StoreOffset {
            dst,
            src,
            offsets: vec![offset]
        });
    }

    pub fn read_address(&self, dst: CoreRegister, basic_block: &mut BasicBlock) {
        let (src, offset) = self.get_offset();
        basic_block.push(ArmIns::AddImm {
            dst,
            src,
            imm: offset
        });
    }

    pub fn copy_to_stack_offset(&self, dst_stack_offset: i32, basic_block: &mut BasicBlock) {
        let transfer_reg = basic_block.claim_register();
        self.read(transfer_reg, basic_block);
        basic_block.push(ArmIns::StoreOffset {
            src: transfer_reg,
            dst: CoreRegister::SP,
            offsets: vec![dst_stack_offset]
        });
        basic_block.free_register(transfer_reg);
    }
}

impl std::fmt::Display for Pointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Pointer::Stack(i) =>
                f.write_fmt(format_args!("Stack<offset={}>", i)),
            Pointer::Heap(s) =>
                f.write_fmt(format_args!("Heap<offset={}>", s))
        }
    }
}

impl std::clone::Clone for Pointer {
    fn clone(&self) -> Self {
        match self {
            Pointer::Stack(i) => Pointer::Stack(*i),
            Pointer::Heap(s) => Pointer::Heap(*s)
        }
    }
}

