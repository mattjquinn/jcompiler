use backend::arm::registers::{CoreRegister, ExtensionRegisterDoublePrecision};
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

    pub fn load_width4(&self, dst: CoreRegister, basic_block: &mut BasicBlock) {
        let (src, offset) = self.get_offset();
        basic_block.push(ArmIns::LoadOffset {
            dst,
            src,
            offsets: vec![offset]
        });
    }

    pub fn store_width4(&self, src: CoreRegister, basic_block: &mut BasicBlock) {
        let (dst, offset) = self.get_offset();
        basic_block.push(ArmIns::StoreOffset {
            dst,
            src,
            offsets: vec![offset]
        });
    }

    pub fn load_width8(&self, dst: ExtensionRegisterDoublePrecision, basic_block: &mut BasicBlock) {
        let (src, offset) = self.get_offset();
        basic_block.push(ArmIns::LoadDoublePrecisionRegister {
            dst, src, offsets: vec![offset] });
    }

    pub fn store_width8(&self, src: ExtensionRegisterDoublePrecision, basic_block: &mut BasicBlock) {
        let (dst, offset) = self.get_offset();
        basic_block.push(ArmIns::StoreDoublePrecisionRegister {
            src, dst, offsets: vec![offset] });
    }

    pub fn load_address(&self, dst: CoreRegister, basic_block: &mut BasicBlock) {
        let (src, offset) = self.get_offset();
        basic_block.push(ArmIns::AddImm {
            dst,
            src,
            imm: offset
        });
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

