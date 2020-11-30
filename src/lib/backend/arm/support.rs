use std::collections::{HashMap};

use std::fmt::{Formatter, Error};

use super::instructions::{ArmIns};

#[derive(Debug)]
pub enum Offset {
    Stack(Type, i32),
    Global(Type, String),
}

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Offset::Stack(ty, i) => f.write_fmt(format_args!(
                "Stack<offset={},type={}>", ty, i)),
            Offset::Global(ty, s) => f.write_fmt(format_args!(
                "Global<offset={},type={}>", ty, s))
        }
    }
}

impl std::clone::Clone for Offset {
    fn clone(&self) -> Self {
        match self {
            Offset::Stack(ty, i) => Offset::Stack(ty.clone(), *i),
            Offset::Global(ty, s) => Offset::Global(ty.clone(), s.clone())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Double,
    Array(u16)  // length of array; no element type (can be mixed)
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        // TODO: there must be a more concise way to do this
        match self {
            Type::Integer => f.write_str("Integer"),
            Type::Double => f.write_str("Double"),
            Type::Array(len) =>
                f.write_fmt(format_args!("Array[{}]", len)),
        }
    }
}

#[derive(Debug)]
pub struct GlobalContext {
    pub global_ident_to_offsets: HashMap<String, Vec<Offset>>
}

impl GlobalContext {

    // TODO: make a note about optimization
    // TODO: history needs to be in an append only array
    pub fn add_and_set_global_ident_offsets(&mut self, ident: &String, offsets: &Vec<Offset>) {
        let mut global_offsets = vec![];
        let mut idx = 0;
        for offset in offsets {
            match offset {
                Offset::Stack(ty, _idx) => {
                    global_offsets.push(Offset::Global(ty.clone(), format!("{}_idx{}", ident, idx)));
                    idx += 1
                }
                Offset::Global(_ty, _ident) => {
                    // TODO: should this be modified in any way?
                    global_offsets.push(offset.clone());
                }
            }
        }
        self.global_ident_to_offsets.insert(ident.clone(), global_offsets);
    }

    pub fn cleanup_stack(&self, postamble: &mut Vec<String>) {
        // TODO: This is incorrect; we need to compute using
        // the future append-only entry list
        if self.global_ident_to_offsets.len() > 0 {
            // Erases the global frame
            postamble.push(format!("{}", ArmIns::AddImm {
                dst:"fp",
                src:"fp",
                imm: (self.global_ident_to_offsets.len() * 4) as i32
            }));
            // Stack pointer must be moved up as well.
            postamble.push(format!("{}",ArmIns::Move {
                dst: "sp",
                src: "fp"
            }));
        }
    }

    pub fn emit_postamble_entries(&self, entries: &mut Vec<String>) {
        for ident in self.global_ident_to_offsets.keys() {
            let offsets = self.global_ident_to_offsets.get(ident).unwrap();
            let mut idx = 0;
            for offset in offsets {
                match offset {
                    Offset::Global(Type::Double, _) => {
                        entries.push(format!(".{}_idx{}: .word {}_idx{}", ident, idx, ident, idx));
                        idx += 1
                    },
                    Offset::Global(Type::Integer, _) => {
                        entries.push(format!(".{}_idx{}: .word {}_idx{}", ident, idx, ident, idx));
                        idx += 1
                    },
                    Offset::Stack(_, _) => {
                        panic!("A stack offset was found in the GlobalContext; this should never happen: {:?}", offset)
                    }
                    _ => panic!("Unsupported offset in emit_preamble_entries: {:?}", offset)
                }
            }
        }
    }

    pub fn emit_preamble_entries(&self, entries: &mut Vec<String>) {
        for ident in self.global_ident_to_offsets.keys() {
            let offsets = self.global_ident_to_offsets.get(ident).unwrap();
            let mut idx = 0;
            for offset in offsets {
                match offset {
                    Offset::Global(Type::Double, _) => {
                        entries.push(format!("{}_idx{}: .word 0", ident, idx));
                        idx += 1
                    },
                    Offset::Global(Type::Integer, _) => {
                        entries.push(format!("{}_idx{}: .word 0", ident, idx));
                        idx += 1
                    },
                    Offset::Stack(_, _) => {
                        panic!("A stack offset was found in the GlobalContext; this should never happen: {:?}", offset)
                    }
                    _ => panic!("Unsupported offset in emit_preamble_entries: {:?}", offset)
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    frame_pointer: i32,
    pub frame_size: i32,
    pub instructions: Vec<ArmIns>,
}

impl BasicBlock {
    pub fn new(frame_size: i32) -> BasicBlock {
        println!("Allocating new basic block with frame size {}", frame_size);
        let mut instructions = vec![
            // Frame pointer starts out at stack pointer.
            ArmIns::Move { dst: "fp", src: "sp" } ];
        let mut subbed = 0;
        while subbed < frame_size {
            let mut to_sub = frame_size - subbed;
            // ctest_mixed_adds_mults appears to be blowing the immediate width...
            if to_sub > 256 {
                to_sub = 256;
            }
            // Expand fp to size of frame.
            instructions.push(ArmIns::SubImm {
                dst: "fp",
                src: "fp",
                imm: to_sub
            });
            subbed += to_sub;
        }
        // Expand sp along with it.
        instructions.push(ArmIns::Move { dst: "sp", src: "fp" });
        BasicBlock {
            frame_size,
            frame_pointer: 0,
            instructions,
        }
    }

    // TODO: should be inaccessible to callers outside of this struct
    pub fn _stack_allocate(&mut self, width: i32) -> i32 {
        if self.frame_pointer + width > self.frame_size {
            panic!("Attempted to allocate entity of width {} which overflows frame size of {}; frame pointer is {}", width, self.frame_size, self.frame_pointer);
        }
        let offset = self.frame_pointer;
        self.frame_pointer += width;
        offset
    }

    pub fn stack_allocate_int(&mut self) -> i32 {
        let offset = self._stack_allocate(4);
        offset
    }

    pub fn stack_allocate_double(&mut self) -> i32 {
        let offset = self._stack_allocate(4);
        offset
    }
}
