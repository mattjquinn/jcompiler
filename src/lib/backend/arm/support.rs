use std::collections::{HashMap};
use linked_hash_set::LinkedHashSet;

use std::fmt::{Formatter, Error};
use ieee754::Ieee754;

use super::instructions::{ArmIns};
use super::ir::{IRNode};
use backend::arm::registers::ArmRegister;
use parser::{MonadicVerb};

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

/**
 * It remains to be seen whether globals should be
 * loaded as ARM global variables, or managed via a special
 * global basic block. The latter would give us the ability
 * to more aggressively free up memory, at the cost of greater
 * complexity.
 */
#[derive(Debug)]
pub struct GlobalContext {
    // TODO: we need a map of idents to lifetime offsets;
    // those lifetime offsets should be written to global memory
    pub global_ident_to_offsets: HashMap<String, Vec<Offset>>
}

impl GlobalContext {

    // TODO: make a note about optimization
    // TODO: history needs to be in an append only array
    pub fn add_and_set_global_ident_offsets(&mut self, ident: &String, offsets: &Vec<Offset>) {
        let mut global_offsets = vec![];
        // TODO: idx is used all wrong here, it must be globally incremented in the context of an ident
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
            postamble.push(format!("{}", ArmIns::AddImmDeprecated {
                dst:"fp",
                src:"fp",
                imm: (self.global_ident_to_offsets.len() * 4) as i32
            }));
            // Stack pointer must be moved up as well.
            postamble.push(format!("{}",ArmIns::MoveDeprecated {
                dst: "sp",
                src: "fp"
            }));
        }
    }

    pub fn emit_postamble_entries(&self, entries: &mut Vec<String>) {
        for ident in self.global_ident_to_offsets.keys() {
            let offsets = self.global_ident_to_offsets.get(ident).unwrap();
            // TODO: idx is used all wrong here, it must be globally incremented in the context of an ident
            let mut idx = 0;
            for offset in offsets {
                match offset {
                    Offset::Global(Type::Double, _) => {
                        entries.push(format!(".{}_idx{}: .long {}_idx{}", ident, idx, ident, idx));
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
            // TODO: idx is used all wrong here, it must be unique per ident
            let mut idx = 0;
            for offset in offsets {
                match offset {
                    Offset::Global(Type::Double, _) => {
                        entries.push(format!("{}_idx{}: .long 0", ident, idx));
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
    pub frame_size: i32,  // TODO: make private
    pub instructions: Vec<ArmIns>, // TODO: make private
    available_registers: LinkedHashSet<ArmRegister>
}

impl BasicBlock {
    pub fn new(frame_size: i32) -> BasicBlock {
        println!("Allocating new basic block with frame size {}", frame_size);
        let mut instructions = vec![
            // Frame pointer starts out at stack pointer.
            ArmIns::MoveDeprecated { dst: "fp", src: "sp" } ];
        let mut subbed = 0;
        while subbed < frame_size {
            let mut to_sub = frame_size - subbed;
            // ctest_mixed_adds_mults appears to be blowing the immediate width...
            if to_sub > 256 {
                to_sub = 256;
            }
            // Expand fp to size of frame.
            instructions.push(ArmIns::SubImmDeprecated {
                dst: "fp",
                src: "fp",
                imm: to_sub
            });
            subbed += to_sub;
        }
        // Expand sp along with it.
        instructions.push(ArmIns::MoveDeprecated { dst: "sp", src: "fp" });
        BasicBlock {
            frame_size,
            frame_pointer: 0,
            instructions,
            available_registers: [
                ArmRegister::R0,
                ArmRegister::R1,
                ArmRegister::R2,
                ArmRegister::R3
            ].iter().cloned().collect()
        }
    }

    // TODO: make private
    pub fn _stack_allocate(&mut self, width: i32) -> i32 {
        if self.frame_pointer + width > self.frame_size {
            panic!("Attempted to allocate entity of width {} which overflows frame size of {}; frame pointer is {}", width, self.frame_size, self.frame_pointer);
        }
        let offset = self.frame_pointer;
        self.frame_pointer += width;
        offset
    }

    // TODO: make private
    pub fn stack_allocate_int(&mut self) -> i32 {
        let offset = self._stack_allocate(4);
        offset
    }

    // TODO: make private
    pub fn stack_allocate_double(&mut self) -> i32 {
        let offset = self._stack_allocate(4);
        offset
    }

    fn claim_register(&mut self) -> ArmRegister {
        match self.available_registers.pop_front() {
            Some(r) => r,
            None => panic!("No register is available to claim.")
        }
    }

    fn free_register(&mut self, reg: ArmRegister) {
        if self.available_registers.contains(&reg) {
            panic!("Attempted to free register {} but it is not claimed.", reg)
        }
        self.available_registers.insert(reg);
    }

    pub fn ir(&mut self, instruction: IRNode) -> Vec<Offset> {
        match instruction {
            IRNode::PushIntegerOntoStack(imm) => {
                let temp_reg = self.claim_register();
                self.instructions.push(ArmIns::MoveImm { imm, dst: temp_reg.clone() });
                let offset = self.stack_allocate_int();
                self.instructions.push(ArmIns::StoreOffset {
                    src: temp_reg.clone(), dst: ArmRegister::FP, offsets: vec![offset]});
                self.free_register(temp_reg);
                vec![Offset::Stack(Type::Integer, offset)]
            },
            IRNode::PushDoublePrecisionFloatOntoStack(num) => {
                let bits = num.bits();
                let hex_rep = format!("{:016x}", bits);
                println!("IEEE754 double hex representation: {}", hex_rep);

                if bits & 0x00000000FFFFFFFF != 0 {
                    // supporting this will require use of an additional register
                    // that is also pushed to the stack
                    panic!("TODO: Support double precision floats with non-zero LSW: {}", hex_rep);
                }

                // due to limited width of immediate, we split the initialization
                // over multiple instructions:
                // TODO: use of i32 here could cause silent truncation
                let msw_upper : i32 = ((bits >> 32) & 0xF0000000) as i32;
                let msw_lower : i32 = ((bits >> 32) & 0x0FFFFFFF) as i32;
                println!("MSW upper: {:x}", msw_upper);
                println!("MSW lower: {:x}", msw_lower);
                let temp_reg = self.claim_register();
                self.instructions.push(ArmIns::MoveImm { imm: msw_upper, dst: temp_reg.clone() });
                self.instructions.push(ArmIns::AddImm {
                    imm: msw_lower, dst: temp_reg.clone(), src: temp_reg.clone() });
                let offset = self.stack_allocate_double();
                self.instructions.push(ArmIns::StoreOffset {
                    dst: ArmRegister::FP, src: temp_reg.clone(), offsets: vec![offset]});
                self.free_register(temp_reg);
                vec![Offset::Stack(Type::Double, offset)]
            }
            IRNode::ApplyMonadicVerbToMemoryOffset(verb, offset) => {
                let memory_offset_idx = match &offset {
                    Offset::Stack(_type, idx) => *idx,
                    Offset::Global(_type, _ident) =>
                        unimplemented!("TODO: Support application of monadic verb to a global variable.")
                };
                let temp_reg = self.claim_register();
                self.instructions.push(ArmIns::LoadOffset {
                    dst: temp_reg.clone(), src: ArmRegister::FP, offsets: vec![memory_offset_idx]});
                match verb {
                    MonadicVerb::Increment => self.instructions.push(ArmIns::AddImm {
                        dst: temp_reg.clone(), src: temp_reg.clone(), imm: 1 }),
                    MonadicVerb::Square => self.instructions.push(ArmIns::Multiply {
                        dst: temp_reg.clone(), src: temp_reg.clone(), mul: temp_reg.clone() }),
                    MonadicVerb::Negate => {
                        let negation_reg = self.claim_register();
                        self.instructions.push(ArmIns::MoveImm { dst: negation_reg.clone(), imm: 0 });
                        // subtract the immediate from 0
                        self.instructions.push(ArmIns::Sub {
                            dst: temp_reg.clone(), src: negation_reg.clone(), sub: temp_reg.clone() });
                        self.free_register(negation_reg);
                    }
                    _ => unimplemented!("TODO: Support monadic verb: {:?}", verb)
                }
                self.instructions.push(ArmIns::StoreOffset {
                    src: temp_reg.clone(), dst: ArmRegister::FP, offsets: vec![memory_offset_idx]});
                self.free_register(temp_reg);
                vec![offset.clone()]   // we return the same offset we were given, because we've updated it in-place on the stack
            }
        }
    }
}
