use std::collections::{HashMap};
use linked_hash_set::LinkedHashSet;

use std::fmt::{Formatter, Error};
use ieee754::Ieee754;

use super::instructions::{ArmIns};
use super::ir::{IRNode};
use backend::arm::registers::ArmRegister;
use parser::{DyadicVerb, MonadicVerb};

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
                    match ty {
                        Type::Double => {
                            // We only want one offset so we use _double to
                            // indicate there is an MSW and LSW that get their own addresses. If we use two
                            // offsets here then we'll attempt to print the MSW and LSW separately.
                            // TODO: Rely on the type system more.
                            global_offsets.push(Offset::Global(ty.clone(), format!("{}_idx{}_double", ident, idx)));
                            idx += 1
                        },
                        Type::Integer => {
                            global_offsets.push(Offset::Global(ty.clone(), format!("{}_idx{}", ident, idx)));
                            idx += 1
                        }
                        Type::Array(_) => {
                            panic!("TODO: Support setting global idents for array of offsets")
                        }
                    };
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
                        entries.push(format!(".{}_idx{}_double_msw: .long {}_idx{}_double_msw", ident, idx, ident, idx));
                        entries.push(format!(".{}_idx{}_double_lsw: .long {}_idx{}_double_lsw", ident, idx, ident, idx));
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
                        entries.push(format!("{}_idx{}_double_msw: .long 0", ident, idx));
                        entries.push(format!("{}_idx{}_double_lsw: .long 0", ident, idx));
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
        // TODO: Do we make this return two offsets, one for MSW and one for LSW?
        let offset = self._stack_allocate(8);
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
                let binary_rep = format!("{:02b}", bits);
                println!("IEEE754 double hex representation of {} is: hex={}, binary={}", num, hex_rep, binary_rep);

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

                // // Move into out_lsw_reg
                // let byte1: i8 = (bits & 0xFF) as i8;
                // // Move into temp_reg, shift left by 8, add to out_lsw_reg
                // let byte2: i8 = ((bits >> 8) & 0xFF) as i8;
                // // Move into temp_reg, shift left by 16, add to out_lsw_reg
                // let byte3: i8 = ((bits >> 16) & 0xFF) as i8;
                // // Move into temp_reg, shift left by 24, add to out_lsw_reg
                // let byte4: i8 = ((bits >> 24) & 0xFF) as i8;
                //
                // // Move into out_msg_reg
                // let byte5: i8 = ((bits >> 32) & 0xFF) as i8;
                // // Move into temp_reg, shift left by 8, add to out_msw_reg
                // let byte6: i8 = ((bits >> 40) & 0xFF) as i8;
                // // Move into temp_reg, shift left by 16, add to out_msw_reg
                // let byte7: i8 = ((bits >> 48) & 0xFF) as i8;
                // // Move into temp_reg, shift left by 24, add to out_msw_reg
                // let byte8: i8 = ((bits >> 48) & 0xFF) as i8;

                println!("MSW upper: {:x}", msw_upper);
                println!("MSW lower: {:x}", msw_lower);
                let temp_reg = self.claim_register();
                self.instructions.push(ArmIns::MoveImm { imm: msw_upper, dst: temp_reg.clone() });
                self.instructions.push(ArmIns::AddImm {
                    imm: msw_lower, dst: temp_reg.clone(), src: temp_reg.clone() });
                let offset = self.stack_allocate_double();
                self.instructions.push(ArmIns::StoreOffset {
                    dst: ArmRegister::FP, src: temp_reg.clone(), offsets: vec![offset]});
                self.instructions.push(ArmIns::MoveImm { imm: 0, dst: temp_reg.clone() });
                self.instructions.push(ArmIns::StoreOffset {
                    dst: ArmRegister::FP, src: temp_reg.clone(), offsets: vec![offset + 4]}); // TODO: if we have a DoubleOffset(msw,lsw) we won't need the manual + 4
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
            },
            IRNode::ApplyDyadicVerbToMemoryOffsets{verb, lhs, rhs} => {
                let lhs_reg = self.claim_register();
                let rhs_reg = self.claim_register();

                let l_type = match lhs {
                    Offset::Stack(ty, i) => {
                        self.instructions.push(
                            ArmIns::LoadOffset { dst: lhs_reg.clone(), src: ArmRegister::FP, offsets: vec![i] });
                        ty
                    },
                    Offset::Global(ty, ident) => {
                        self.instructions.push(
                            // TODO: make this be ArmIns::LoadIdentifierAddress
                            ArmIns::Load { dst: lhs_reg.clone(), src: format!(".{}", ident).to_string() });
                        self.instructions.push(
                            // TODO: make this be ArmIns::LoadFromDereferencedAddressInRegister
                            ArmIns::Load { dst: lhs_reg.clone(), src: format!("[{}]", lhs_reg).to_string() });
                        ty
                    }
                };
                let r_type = match rhs {
                    Offset::Stack(ty, i) => {
                        self.instructions.push(
                            ArmIns::LoadOffset { dst: rhs_reg.clone(), src: ArmRegister::FP, offsets: vec![i] });
                        ty
                    }
                    // TODO: it's not clear why this is unimplemented when we seem to implement it for the left-hand side above
                    Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
                };
                let unified_type = unify_types(&l_type, &r_type);
                match verb {
                    DyadicVerb::Plus =>
                        self.instructions.push(
                            ArmIns::Add { dst: rhs_reg.clone(), src: lhs_reg.clone(), add: rhs_reg.clone() }),
                    DyadicVerb::Times =>
                        self.instructions.push(
                            ArmIns::Multiply { dst: rhs_reg.clone(), src: lhs_reg.clone(), mul: rhs_reg.clone() }),
                    DyadicVerb::Minus =>
                        self.instructions.push(
                            ArmIns::Sub { dst: rhs_reg.clone(), src: lhs_reg.clone(), sub: rhs_reg.clone() }),
                    DyadicVerb::LessThan => {
                        self.instructions.push(
                            ArmIns::Compare { lhs: lhs_reg.clone(), rhs: rhs_reg.clone() });
                        self.instructions.push(
                            ArmIns::MoveLT { dst: rhs_reg.clone(), src: 1 });
                        self.instructions.push(
                            ArmIns::MoveGE { dst: rhs_reg.clone(), src: 0 });
                    },
                    DyadicVerb::Equal => {
                        self.instructions.push(
                            ArmIns::Compare { lhs: lhs_reg.clone(), rhs: rhs_reg.clone() });
                        self.instructions.push(
                            ArmIns::MoveEQ { dst: rhs_reg.clone(), src: 1 });
                        self.instructions.push(
                            ArmIns::MoveNE { dst: rhs_reg.clone(), src: 0 });
                    }
                    DyadicVerb::LargerThan => {
                        self.instructions.push(
                            ArmIns::Compare { lhs: lhs_reg.clone(), rhs: rhs_reg.clone() });
                        self.instructions.push(
                            ArmIns::MoveGT { dst: rhs_reg.clone(), src: 1 });
                        self.instructions.push(
                            ArmIns::MoveLE { dst: rhs_reg.clone(), src: 0 });
                    }
                    _ => panic!("Not ready to compile dyadic verb: {:?}", verb)
                }
                let dest_offset = self._stack_allocate(4);
                self.instructions.push(ArmIns::StoreOffset {
                    src: rhs_reg.clone(), dst: ArmRegister::FP, offsets: vec![dest_offset] });

                self.free_register(lhs_reg);
                self.free_register(rhs_reg);

                vec![Offset::Stack(unified_type, dest_offset)]
            },
            IRNode::ReduceMemoryOffsets(verb, expr_offsets) => {
                // Initialize the accumulator to expr's last offset value
                let accum_reg = self.claim_register();
                let accum_offset = expr_offsets.last().unwrap();
                match &accum_offset {
                    Offset::Stack(_type, i) =>
                        self.instructions.push(ArmIns::LoadOffset {
                            dst: accum_reg.clone(), src: ArmRegister::FP, offsets: vec![*i] }),
                    Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
                }

                // Accumulate from right to left.
                let operand_reg = self.claim_register();
                for offset_idx in expr_offsets[0..expr_offsets.len()-1].iter().rev() {
                    match offset_idx {
                        Offset::Stack(_type, i) =>
                            self.instructions.push(ArmIns::LoadOffset {
                                dst: operand_reg.clone(), src: ArmRegister::FP, offsets: vec![*i] }),
                        Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
                    };
                    match verb {
                        DyadicVerb::Plus => {
                            self.instructions.push(ArmIns::Add {
                                dst: accum_reg.clone(),
                                src: operand_reg.clone(),
                                add: accum_reg.clone()
                            });
                        },
                        DyadicVerb::Minus => {
                            self.instructions.push(ArmIns::Sub {
                                dst: accum_reg.clone(),
                                src: operand_reg.clone(),
                                sub: accum_reg.clone()
                            });
                        },
                        DyadicVerb::Times => {
                            self.instructions.push(ArmIns::Multiply {
                                dst: accum_reg.clone(),
                                src: operand_reg.clone(),
                                mul: accum_reg.clone()
                            });
                        },
                        _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb)
                    }
                }
                self.free_register(operand_reg);

                // Store the accumulator in expr's first offset, and return that
                // single offset here.
                match &accum_offset {
                    Offset::Stack(_type, i) =>
                        self.instructions.push(ArmIns::StoreOffset {
                            src: accum_reg.clone(), dst: ArmRegister::FP, offsets: vec![*i] }),
                    Offset::Global(_type, _ident) => unimplemented!("TODO: Support store to global.")
                };

                self.free_register(accum_reg.clone());
                vec![accum_offset.clone()]
            },
            IRNode::AssignMemoryOffsetsToGlobal{ident, offsets: expr_offsets} => {
                let mut out_offsets = vec![];
                let mut idx = 0;
                for offset in expr_offsets.iter() {
                    match offset {
                        Offset::Stack(ty, i) => {
                            match ty {
                                Type::Integer => {
                                    let value_reg = self.claim_register();
                                    let indirection_reg = self.claim_register();
                                    self.instructions.push(ArmIns::LoadOffset {
                                        dst: value_reg.clone(),
                                        src: ArmRegister::FP,
                                        offsets: vec![*i]
                                    });
                                    // This load/store sequence looks confusing; it is putting the
                                    // address of the element in the global var, into which the value
                                    // will be placed, into the indirection register, and when the
                                    // store of the value occurs, the value will "pass through"
                                    // the address in the indirection register to end up in the global
                                    // address space.
                                    self.instructions.push(ArmIns::Load {
                                        src: format!(".{}_idx{}", ident, idx),
                                        dst: indirection_reg.clone()
                                    });
                                    idx += 1;
                                    self.instructions.push(ArmIns::Store {
                                        src: value_reg.clone(),
                                        dst: indirection_reg.clone()
                                    });
                                    self.free_register(value_reg);
                                    self.free_register(indirection_reg);
                                },
                                Type::Double => {
                                    let msw_reg = self.claim_register();
                                    let lsw_reg = self.claim_register();
                                    let indirection_reg = self.claim_register();
                                    self.instructions.push(ArmIns::LoadOffset {
                                        dst: msw_reg.clone(),
                                        src: ArmRegister::FP,
                                        offsets: vec![*i]
                                    });
                                    self.instructions.push(ArmIns::LoadOffset {
                                        dst: lsw_reg.clone(),
                                        src: ArmRegister::FP,
                                        offsets: vec![*i + 4] // TODO: if statically type the offset as a DoubleOffset(msw, lsw) we won't have to do this
                                    });
                                    // This load/store sequence looks confusing; it is putting the
                                    // address of the element in the global var, into which the value
                                    // will be placed, into the indirection register, and when the
                                    // store of the value occurs, the value will "pass through"
                                    // the address in the indirection register to end up in the global
                                    // address space.
                                    self.instructions.push(ArmIns::Load {
                                        src: format!(".{}_idx{}_double_msw", ident, idx),
                                        dst: indirection_reg.clone()
                                    });
                                    self.instructions.push(ArmIns::Store {
                                        src: msw_reg.clone(),
                                        dst: indirection_reg.clone()
                                    });
                                    self.instructions.push(ArmIns::Load {
                                        src: format!(".{}_idx{}_double_lsw", ident, idx),
                                        dst: indirection_reg.clone()
                                    });
                                    self.instructions.push(ArmIns::Store {
                                        src: lsw_reg.clone(),
                                        dst: indirection_reg.clone()
                                    });
                                    idx += 1;
                                    self.free_register(msw_reg);
                                    self.free_register(lsw_reg);
                                    self.free_register(indirection_reg);
                                },
                                Type::Array(_) =>
                                    panic!("TODO: Load stack array offsets into value registers")
                            }
                        },
                        Offset::Global(ty, global_ident) => {
                            match ty {
                                Type::Integer => {
                                    let value_reg = self.claim_register();
                                    let indirection_reg = self.claim_register();
                                    // TODO: make this be ArmIns::LoadIdentifierAddress
                                    self.instructions.push(ArmIns::Load {
                                        dst: value_reg.clone(),
                                        src: format!("{}", global_ident).to_string()
                                    });
                                    // TODO: make this be ArmIns::LoadFromDereferencedAddressInRegister
                                    self.instructions.push(ArmIns::Load {
                                        dst: value_reg.clone(),
                                        src: format!("[{}]", value_reg.clone()).to_string(),
                                    });
                                    // This load/store sequence looks confusing; it is putting the
                                    // address of the element in the global var, into which the value
                                    // will be placed, into the indirection register, and when the
                                    // store of the value occurs, the value will "pass through"
                                    // the address in the indirection register to end up in the global
                                    // address space.
                                    self.instructions.push(ArmIns::Load {
                                        src: format!(".{}_idx{}", ident, idx),
                                        dst: indirection_reg.clone()
                                    });
                                    idx += 1;
                                    self.instructions.push(ArmIns::Store {
                                        src: value_reg.clone(),
                                        dst: indirection_reg.clone()
                                    });
                                    self.free_register(value_reg);
                                    self.free_register(indirection_reg);
                                }
                                Type::Double =>
                                    panic!("TODO: Load global MSW and LSW offsets into value registers"),
                                Type::Array(_) =>
                                    panic!("TODO: Load global array offsets into value registers")
                            }
                        }
                    };
                    println!("In GlobalVarAssgmt, adding offset: {:?}", offset);
                    out_offsets.push(offset.clone())
                }
                out_offsets
            }
        }
    }
}

pub fn unify_types(l_type: &Type, r_type: &Type) -> Type {
    match (l_type, r_type) {
        (Type::Integer, Type::Integer) => Type::Integer,
        (Type::Double, Type::Double) => Type::Double,
        _ => unimplemented!("TODO: Support unification of type {} with {}", l_type, r_type)
    }
}

