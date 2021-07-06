use std::collections::{HashMap};
use linked_hash_set::LinkedHashSet;

use std::fmt::{Formatter, Error};
use ieee754::Ieee754;
use std::fs::File;
use std::io::Write;

use super::instructions::{ArmIns};
use super::ir::{IRNode};
use backend::arm::registers::ArmRegister;
use parser::{DyadicVerb, MonadicVerb};
use backend::arm::registers::ArmRegister::{R2, FP};

#[derive(Debug)]
pub enum Pointer {
    Stack(i32),  // a (negative) offset from sp into the line-extent stack
    Heap(i32),  // a (negative) offset from fp into the process-extent heap
}

impl Pointer {

    fn get_offset(&self) -> (ArmRegister, i32) {
        match self {
            Pointer::Stack(offset) => (ArmRegister::SP, *offset),
            Pointer::Heap(offset) => (ArmRegister::FP, *offset)
        }
    }

    pub fn load(&self, dst: ArmRegister, basic_block: &mut BasicBlock) {
        let (src, offset) = self.get_offset();
        basic_block.push(ArmIns::LoadOffset {
            dst,
            src,
            offsets: vec![offset]
        });
    }

    pub fn store(&self, src: ArmRegister, basic_block: &mut BasicBlock) {
        let (dst, offset) = self.get_offset();
        basic_block.push(ArmIns::StoreOffset {
            dst,
            src,
            offsets: vec![offset]
        });
    }
}

impl std::fmt::Display for Pointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Pointer::Stack(i) => f.write_fmt(format_args!(
                "Stack<offset={}>", i)),
            Pointer::Heap(s) => f.write_fmt(format_args!(
                "Heap<offset={}>", s))
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

#[derive(Debug, Clone)]
pub enum TypedValue {
    Integer(Pointer),
    Double{ msw: Pointer, lsw: Pointer }
}

impl TypedValue {
    pub fn persist_to_heap(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> TypedValue {
        match self {
            TypedValue::Integer(src) => {
                match src {
                    Pointer::Heap(_) => self.clone(),  // already on the heap, nothing to do
                    Pointer::Stack(_) => {
                        let out = globalctx.heap_allocate_int();
                        match &out {
                            TypedValue::Integer(dst) => {
                                let transfer_reg = basic_block.claim_register();
                                src.load(transfer_reg, basic_block);
                                dst.store(transfer_reg, basic_block);
                                basic_block.free_register(transfer_reg);
                            },
                            _ => panic!("Unreachable")
                        }
                        out
                    }
                }
            },
            TypedValue::Double { msw, lsw } => {
                match (msw, lsw) {
                    (Pointer::Heap(_), Pointer::Heap(_)) => self.clone(), // TODO: increment refcount
                    (Pointer::Heap(_), Pointer::Stack(_)) => {
                        panic!("TODO: persist only stack Double MSW to Heap")
                    },
                    (Pointer::Stack(_), Pointer::Heap(_)) => {
                        panic!("TODO: persist only stack Double LSW to Heap")
                    }
                    (Pointer::Stack(_), Pointer::Stack(_)) => {
                        let out = globalctx.heap_allocate_double();
                        match &out {
                            TypedValue::Double { msw: msw_heap, lsw: lsw_heap} => {
                                let transfer_reg = basic_block.claim_register();
                                msw.load(transfer_reg, basic_block);
                                msw_heap.store(transfer_reg, basic_block);
                                lsw.load(transfer_reg, basic_block);
                                lsw_heap.store(transfer_reg, basic_block);
                                basic_block.free_register(transfer_reg);
                                out
                            },
                            _ => panic!("Unreachable")
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct GlobalContext {
    ident_map: HashMap<String, Vec<TypedValue>>,
    heap_pointer: i32,
    heap_size: i32
}

impl GlobalContext {

    pub fn new() -> GlobalContext {
        GlobalContext {
            ident_map: HashMap::new(),
            heap_size: 128,
            heap_pointer: 0
        }
    }

    pub fn write_preamble_to_file(&self, assembly_file: &mut File) {
        let mut preamble = vec![];
        let mut subbed = 0;
        // The FP should start out where the SP is.
        preamble.push(ArmIns::Move { src: ArmRegister::SP, dst: ArmRegister::FP });
        // Expand heap.
        while subbed < self.heap_size {
            let mut to_sub = self.heap_size - subbed;
            // TODO: rather than iterating, we can calculate the appropriate mask to use here
            // so as to do this in a single step; same with mirror code in the cleanup function.
            if to_sub > 256 {
                to_sub = 256;
            }
            preamble.push(ArmIns::SubImm {
                dst: ArmRegister::FP,
                src: ArmRegister::FP,
                imm: to_sub
            });
            subbed += to_sub;
        }
        // Stacks must appear after the heap, so we adjust the SP now.
        preamble.push(ArmIns::Move { src: ArmRegister::FP, dst: ArmRegister::SP });
        for instr in preamble {
            writeln!(assembly_file, "{}", instr).expect("write failure");
        }
    }

    pub fn write_postamble_to_file(&self, assembly_file: &mut File) {
        let mut postamble = vec![];
        let mut added = 0;
        // Cleanup heap.
        while added < self.heap_size {
            let mut to_add = self.heap_size - added;
            // TODO: rather than iterating, we can calculate the appropriate mask to use here
            // so as to do this in a single step; same with mirror code in the cleanup function.
            if to_add > 256 {
                to_add = 256;
            }
            postamble.push(ArmIns::AddImm {
                dst: ArmRegister::FP,
                src: ArmRegister::FP,
                imm: to_add
            });
            added += to_add;
        }
        // Reset SP as well
        postamble.push(ArmIns::Move { src: ArmRegister::FP, dst: ArmRegister::SP });
        for instr in postamble {
            writeln!(assembly_file, "{}", instr).expect("write failure");
        }
    }

    fn _heap_allocate(&mut self, width: i32) -> i32 {
        if self.heap_pointer + width > self.heap_size {
            panic!("Heap allocation failed; adding width {} will overflow heap size of {}; heap pointer is {}", width, self.heap_size, self.heap_pointer);
        }
        let out = self.heap_pointer;
        self.heap_pointer += width;
        out
    }

    pub fn heap_allocate_int(&mut self) -> TypedValue {
        let offset = self._heap_allocate(4);
        TypedValue::Integer(Pointer::Heap(offset))
    }

    pub fn heap_allocate_double(&mut self) -> TypedValue {
        let msw = Pointer::Heap(self._heap_allocate(4));
        let lsw = Pointer::Heap(self._heap_allocate(4));
        TypedValue::Double{msw, lsw}
    }

    pub fn set_ident_values(&mut self, ident: &String, values: &Vec<TypedValue>) {
        for value in values {
            // Caller is responsible for migrating values to Heap first if necessary; enforce that here.
            match value {
                TypedValue::Integer(pointer) => {
                    match pointer {
                        Pointer::Stack(_) => panic!("Attempted to make a Stack pointer global."),
                        Pointer::Heap(_) => {} // OK
                    }
                },
                TypedValue::Double {msw, lsw} => {
                    match (msw, lsw) {
                        (Pointer::Stack(_), Pointer::Stack(_)) => panic!("Attempted to make a Stack MSW and LSW global."),
                        (Pointer::Stack(_), Pointer::Heap(_)) => panic!("Attempted to make a Stack MSW global."),
                        (Pointer::Heap(_), Pointer::Stack(_)) => panic!("Attempted to make a Stack LSW global."),
                        (Pointer::Heap(_), Pointer::Heap(_)) => {} // OK
                    }
                }
            }
        }

        // TODO: There could be previous values in the heap that we lose by overwriting
        // here. We should implement refcounting so that we can deallocate those that will
        // no longer be referred to after this insert.
        self.ident_map.insert(ident.clone(), values.clone());
    }

    pub fn get_ident_values(&mut self, ident: &String) -> Vec<TypedValue> {
        self.ident_map.get(ident).unwrap().clone()
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    stack_pointer: i32,
    stack_size: i32,
    instructions: Vec<ArmIns>,
    available_registers: LinkedHashSet<ArmRegister>
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        let stack_size = 128;  // TODO: temporary default
        println!("Allocating new basic block with stack size {}", stack_size);
        let mut instructions = vec![];
        let mut subbed = 0;
        // Expand stack.
        while subbed < stack_size {
            let mut to_sub = stack_size - subbed;
            // the stack size for ctest_mixed_adds_mults appears to be blowing the immediate width...
            // TODO: rather than iterating, we can calculate the appropriate mask to use here
            // so as to do this in a single step; same with mirror code in the cleanup function.
            if to_sub > 256 {
                to_sub = 256;
            }
            instructions.push(ArmIns::SubImm {
                dst: ArmRegister::SP,
                src: ArmRegister::SP,
                imm: to_sub
            });
            subbed += to_sub;
        }
        BasicBlock {
            stack_pointer: 0,
            stack_size,
            instructions,
            available_registers: [
                ArmRegister::R0,
                ArmRegister::R1,
                ArmRegister::R2,
                ArmRegister::R3
            ].iter().cloned().collect()
        }
    }

    pub fn cleanup(&mut self) {
        let mut added = 0;
        while added < self.stack_size {
            let mut to_add = self.stack_size - added;
            // ctest_mixed_adds_mults appears to be blowing the immediate width...
            // TODO: rather than iterating, we can calculate the appropriate mask to use here
            // so as to do this in a single step; same with mirror code in the cleanup function.
            if to_add > 256 {
                to_add = 256;
            }
            self.instructions.push(ArmIns::AddImm {
                dst: ArmRegister::SP,
                src: ArmRegister::SP,
                imm: to_add
            });
            added += to_add;
        }
    }

    pub fn push(&mut self, instruction: ArmIns) {
        self.instructions.push(instruction)
    }

    pub fn write_instructions_to_file(&self, assembly_file: &mut File) {
        for instr in &self.instructions {
            writeln!(assembly_file, "{}", instr).expect("write failure");
        }
    }

    fn _stack_allocate(&mut self, width: i32) -> i32 {
        if self.stack_pointer + width > self.stack_size {
            panic!("Stack allocation failed; adding width {} will overflow stack size of {}; stack pointer is {}", width, self.stack_size, self.stack_pointer);
        }
        let out = self.stack_pointer;
        self.stack_pointer += width;
        out
    }

    fn stack_allocate_int(&mut self) -> TypedValue {
        let offset = self._stack_allocate(4);
        TypedValue::Integer(Pointer::Stack(offset))
    }

    fn stack_allocate_double(&mut self) -> TypedValue {
        let msw = Pointer::Stack(self._stack_allocate(4));
        let lsw = Pointer::Stack(self._stack_allocate(4));
        TypedValue::Double{msw, lsw}
    }

    pub fn claim_register(&mut self) -> ArmRegister {
        match self.available_registers.pop_front() {
            Some(r) => r,
            None => panic!("No register is available to claim.")
        }
    }

    pub fn free_register(&mut self, reg: ArmRegister) {
        if self.available_registers.contains(&reg) {
            panic!("Attempted to free register {} but it is not claimed.", reg)
        }
        self.available_registers.insert(reg);
    }

    pub fn ir(&mut self,
              instruction: IRNode,
              globalctx: &mut GlobalContext) -> Vec<TypedValue> {
        match instruction {
            IRNode::PushIntegerOntoStack(imm) => {
                let val = self.stack_allocate_int();
                match &val {
                    TypedValue::Integer(dst) => {
                        let transfer_reg = self.claim_register();
                        self.push(ArmIns::MoveImm { imm, dst: transfer_reg.clone() });
                        dst.store(transfer_reg.clone(), self);
                        self.free_register(transfer_reg);
                    },
                    _ => panic!("Unreachable")
                }
                vec![val]
            },
            IRNode::PushDoublePrecisionFloatOntoStack(num) => {
                let bits = num.bits();
                let hex_rep = format!("{:x}", bits);
                let binary_rep = format!("{:064b}", bits);
                println!("IEEE754 double hex representation of {} is: hex={}, binary={}", num, hex_rep, binary_rep);

                let out = self.stack_allocate_double();
                match &out {
                    TypedValue::Double { msw, lsw } => {
                        // Note: due to limited width of ARM's MOV immediate field,
                        // we split the initialization of both MSW and LWS over multiple
                        // per-byte instructions; probably could be optimized.
                        { // LSW
                            let lsw_reg = self.claim_register();
                            {
                                let half1 = (bits & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half1, dst: lsw_reg.clone() });
                            }
                            {
                                let temp_reg = self.claim_register();
                                let half2 = ((bits >> 16) & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half2, dst: temp_reg.clone() });
                                self.push(ArmIns::LeftShift {
                                    src: temp_reg.clone(), dst: temp_reg.clone(), n_bits: 16 });
                                self.push(ArmIns::Add {
                                    dst: lsw_reg.clone(), src: lsw_reg.clone(), add: temp_reg.clone() });
                                self.free_register(temp_reg);
                            }
                            lsw.store(lsw_reg, self);
                            self.free_register(lsw_reg);
                        }
                        { // MSW
                            let msw_reg = self.claim_register();
                            {
                                let half1 = ((bits >> 32) & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half1, dst: msw_reg.clone() });
                            }
                            {
                                let temp_reg = self.claim_register();
                                let half2 = ((bits >> 48) & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half2, dst: temp_reg.clone() });
                                self.push(ArmIns::LeftShift {
                                    src: temp_reg.clone(), dst: temp_reg.clone(), n_bits: 16 });
                                self.push(ArmIns::Add {
                                    dst: msw_reg.clone(), src: msw_reg.clone(), add: temp_reg.clone() });
                                self.free_register(temp_reg);
                            }
                            msw.store(msw_reg, self);
                            self.free_register(msw_reg);
                        }
                        vec![out]
                    },
                    _ => panic!("Unreachable")
                }
            }
            IRNode::ApplyMonadicVerbToTypedValue(verb, value) => {
                match verb {
                    MonadicVerb::Increment => {
                        match &value {
                            TypedValue::Integer(pointer) => {
                                let increment_reg = self.claim_register();
                                pointer.load(increment_reg, self);
                                self.push(ArmIns::AddImm {
                                    dst: increment_reg,
                                    src: increment_reg,
                                    imm: 1
                                });
                                pointer.store(increment_reg, self);
                                self.free_register(increment_reg);
                                vec![value]   // we've updated the existing value in-place
                            },
                            TypedValue::Double{msw, lsw} =>
                                panic!("TODO: Support monadic increment of typed value: {:?}", value)
                        }
                    },
                    MonadicVerb::Square => {
                        match &value {
                            TypedValue::Integer(pointer) => {
                                let multiply_reg = self.claim_register();
                                pointer.load(multiply_reg, self);
                                self.instructions.push(ArmIns::Multiply {
                                    dst: multiply_reg,
                                    src: multiply_reg,
                                    mul: multiply_reg,
                                });
                                pointer.store(multiply_reg, self);
                                self.free_register(multiply_reg);
                                vec![value]   // we've updated the existing value in-place
                            },
                            TypedValue::Double{msw, lsw} =>
                                panic!("TODO: Support monadic square of typed value: {:?}", value)
                        }
                    },
                    MonadicVerb::Negate => {
                        match &value {
                            TypedValue::Integer(pointer) => {
                                let temp_reg = self.claim_register();
                                let negation_reg = self.claim_register();
                                pointer.load(temp_reg, self);
                                self.instructions.push(ArmIns::MoveImm {
                                    dst: negation_reg, imm: 0 });
                                // subtract the immediate from 0
                                self.instructions.push(ArmIns::Sub {
                                    dst: temp_reg,
                                    src: negation_reg,
                                    sub: temp_reg
                                });
                                pointer.store(temp_reg, self);
                                self.free_register(temp_reg);
                                self.free_register(negation_reg);
                                vec![value]   // we've updated the existing offset in-place on the stack
                            },
                            TypedValue::Double{msw, lsw} => {
                                let msw_reg = self.claim_register();
                                msw.load(msw_reg, self);
                                self.instructions.push(ArmIns::ExclusiveOr {
                                    dst: msw_reg,
                                    src: msw_reg,
                                    operand: 0x80000000
                                }); // flip the MSB of the MSW (2's complement sign bit)
                                msw.store(msw_reg, self);
                                self.free_register(msw_reg);
                                vec![value]   // we've updated the existing offset in-place on the stack
                            }
                        }
                    },
                    MonadicVerb::Ceiling => {
                        match &value {
                            TypedValue::Integer(_) => vec![value], // nothing to do
                            TypedValue::Double { msw, lsw } => {
                                // We could call "modf" in math.h, but we don't actually need the fractional part.
                                // TODO: we should be claiming these registers by name from the BasicBlock
                                msw.load(ArmRegister::R1, self); // the MSW is expected in r1
                                lsw.load(ArmRegister::R0, self); // the MSW is expected in r0
                                self.push(ArmIns::BranchAndLink { addr: "jcompiler_ceiling" });
                                let new_value = self.stack_allocate_int();
                                match &new_value {
                                    TypedValue::Integer(pointer) => {
                                        pointer.store(ArmRegister::R0, self);
                                    },
                                    _ => panic!("Unreachable")
                                }
                                // TODO: we should decrement the old value's refcount so
                                // we can reuse its space later on.
                                vec![new_value]
                            }
                            _ => panic!("TODO: support monadic ceiling for typed value: {:?}", value)
                        }
                    }
                    _ => unimplemented!("TODO: Support monadic verb: {:?}", verb)
                }
            },
            IRNode::ApplyDyadicVerbToTypedValues {verb, lhs, rhs} => {

                match (&lhs, &rhs) {
                    (TypedValue::Integer(lhsptr), TypedValue::Integer(rhsptr)) => {
                        let lhs_reg = self.claim_register();
                        let rhs_reg = self.claim_register();
                        lhsptr.load(lhs_reg, self);
                        rhsptr.load(rhs_reg, self);
                        match verb {
                            DyadicVerb::Plus =>
                                self.instructions.push(
                                    ArmIns::Add { dst: rhs_reg, src: lhs_reg, add: rhs_reg }),
                            DyadicVerb::Times =>
                                self.instructions.push(
                                    ArmIns::Multiply { dst: rhs_reg, src: lhs_reg, mul: rhs_reg }),
                            DyadicVerb::Minus =>
                                self.instructions.push(
                                    ArmIns::Sub { dst: rhs_reg, src: lhs_reg, sub: rhs_reg }),
                            DyadicVerb::LessThan => {
                                self.instructions.push(
                                    ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                                self.instructions.push(
                                    ArmIns::MoveLT { dst: rhs_reg, src: 1 });
                                self.instructions.push(
                                    ArmIns::MoveGE { dst: rhs_reg, src: 0 });
                            },
                            DyadicVerb::Equal => {
                                self.instructions.push(
                                    ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                                self.instructions.push(
                                    ArmIns::MoveEQ { dst: rhs_reg, src: 1 });
                                self.instructions.push(
                                    ArmIns::MoveNE { dst: rhs_reg, src: 0 });
                            }
                            DyadicVerb::LargerThan => {
                                self.instructions.push(
                                    ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                                self.instructions.push(
                                    ArmIns::MoveGT { dst: rhs_reg, src: 1 });
                                self.instructions.push(
                                    ArmIns::MoveLE { dst: rhs_reg, src: 0 });
                            }
                            _ => panic!("Not ready to compile dyadic verb: {:?}", verb)
                        }
                        let out_value = self.stack_allocate_int();
                        match &out_value {
                            TypedValue::Integer(pointer) => pointer.store(rhs_reg, self),
                            _ => panic!("Unreachable")
                        }
                        self.free_register(lhs_reg);
                        self.free_register(rhs_reg);
                        vec![out_value]
                    },
                    (_, _) => panic!("TODO: Support dyadic verb on types lhs={:?}, rhs={:?}", lhs, rhs)
                }
            },
            IRNode::ReduceTypedValues(verb, expr_offsets) => {
                panic!("TODO: Support ReduceTypedValues");
                // // Initialize the accumulator to expr's last offset value
                // let accum_reg = self.claim_register();
                // let accum_offset = expr_offsets.last().unwrap();
                // match &accum_offset {
                //     Pointer::Stack(_type, i) =>
                //         self.instructions.push(ArmIns::LoadOffset {
                //             dst: accum_reg.clone(), src: ArmRegister::FP, offsets: vec![*i] }),
                //     Pointer::Heap(_type, _ident) => unimplemented!("TODO: Support load from global.")
                // }
                //
                // // Accumulate from right to left.
                // let operand_reg = self.claim_register();
                // for offset_idx in expr_offsets[0..expr_offsets.len()-1].iter().rev() {
                //     match offset_idx {
                //         Pointer::Stack(_type, i) =>
                //             self.instructions.push(ArmIns::LoadOffset {
                //                 dst: operand_reg.clone(), src: ArmRegister::FP, offsets: vec![*i] }),
                //         Pointer::Heap(_type, _ident) => unimplemented!("TODO: Support load from global.")
                //     };
                //     match verb {
                //         DyadicVerb::Plus => {
                //             self.instructions.push(ArmIns::Add {
                //                 dst: accum_reg.clone(),
                //                 src: operand_reg.clone(),
                //                 add: accum_reg.clone()
                //             });
                //         },
                //         DyadicVerb::Minus => {
                //             self.instructions.push(ArmIns::Sub {
                //                 dst: accum_reg.clone(),
                //                 src: operand_reg.clone(),
                //                 sub: accum_reg.clone()
                //             });
                //         },
                //         DyadicVerb::Times => {
                //             self.instructions.push(ArmIns::Multiply {
                //                 dst: accum_reg.clone(),
                //                 src: operand_reg.clone(),
                //                 mul: accum_reg.clone()
                //             });
                //         },
                //         _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb)
                //     }
                // }
                // self.free_register(operand_reg);
                //
                // // Store the accumulator in expr's first offset, and return that
                // // single offset here.
                // match &accum_offset {
                //     Pointer::Stack(_type, i) =>
                //         self.instructions.push(ArmIns::StoreOffset {
                //             src: accum_reg.clone(), dst: ArmRegister::FP, offsets: vec![*i] }),
                //     Pointer::Heap(_type, _ident) => unimplemented!("TODO: Support store to global.")
                // };
                //
                // self.free_register(accum_reg.clone());
                // vec![accum_offset.clone()]
            },
            IRNode::AssignTypedValuesToGlobal {ident, values} => {
                let mut out = vec![];
                let mut idx = 0;
                for value in values.iter() {
                    out.push(value.persist_to_heap(self, globalctx));
                    // match offset {
                    //     Pointer::Stack(ty, i) => {
                    //         match ty {
                    //             Type::Integer => {
                    //                 let value_reg = self.claim_register();
                    //                 let indirection_reg = self.claim_register();
                    //                 self.instructions.push(ArmIns::LoadOffset {
                    //                     dst: value_reg.clone(),
                    //                     src: ArmRegister::FP,
                    //                     offsets: vec![*i]
                    //                 });
                    //                 // This load/store sequence looks confusing; it is putting the
                    //                 // address of the element in the global var, into which the value
                    //                 // will be placed, into the indirection register, and when the
                    //                 // store of the value occurs, the value will "pass through"
                    //                 // the address in the indirection register to end up in the global
                    //                 // address space.
                    //                 self.instructions.push(ArmIns::Load {
                    //                     src: format!(".{}_idx{}", ident, idx),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 idx += 1;
                    //                 self.instructions.push(ArmIns::Store {
                    //                     src: value_reg.clone(),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 self.free_register(value_reg);
                    //                 self.free_register(indirection_reg);
                    //             },
                    //             Type::Double => {
                    //                 let msw_reg = self.claim_register();
                    //                 let lsw_reg = self.claim_register();
                    //                 let indirection_reg = self.claim_register();
                    //                 self.instructions.push(ArmIns::LoadOffset {
                    //                     dst: msw_reg.clone(),
                    //                     src: ArmRegister::FP,
                    //                     offsets: vec![*i]
                    //                 });
                    //                 self.instructions.push(ArmIns::LoadOffset {
                    //                     dst: lsw_reg.clone(),
                    //                     src: ArmRegister::FP,
                    //                     offsets: vec![*i + 4] // TODO: if statically type the offset as a DoubleOffset(msw, lsw) we won't have to do this
                    //                 });
                    //                 // This load/store sequence looks confusing; it is putting the
                    //                 // address of the element in the global var, into which the value
                    //                 // will be placed, into the indirection register, and when the
                    //                 // store of the value occurs, the value will "pass through"
                    //                 // the address in the indirection register to end up in the global
                    //                 // address space.
                    //                 self.instructions.push(ArmIns::Load {
                    //                     src: format!(".{}_idx{}_double_msw", ident, idx),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 self.instructions.push(ArmIns::Store {
                    //                     src: msw_reg.clone(),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 self.instructions.push(ArmIns::Load {
                    //                     src: format!(".{}_idx{}_double_lsw", ident, idx),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 self.instructions.push(ArmIns::Store {
                    //                     src: lsw_reg.clone(),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 idx += 1;
                    //                 self.free_register(msw_reg);
                    //                 self.free_register(lsw_reg);
                    //                 self.free_register(indirection_reg);
                    //             },
                    //             Type::Array(_) =>
                    //                 panic!("TODO: Load stack array offsets into value registers")
                    //         }
                    //     },
                    //     Pointer::Heap(ty, global_ident) => {
                    //         match ty {
                    //             Type::Integer => {
                    //                 let value_reg = self.claim_register();
                    //                 let indirection_reg = self.claim_register();
                    //                 // TODO: make this be ArmIns::LoadIdentifierAddress
                    //                 self.instructions.push(ArmIns::Load {
                    //                     dst: value_reg.clone(),
                    //                     src: format!("{}", global_ident).to_string()
                    //                 });
                    //                 // TODO: make this be ArmIns::LoadFromDereferencedAddressInRegister
                    //                 self.instructions.push(ArmIns::Load {
                    //                     dst: value_reg.clone(),
                    //                     src: format!("[{}]", value_reg.clone()).to_string(),
                    //                 });
                    //                 // This load/store sequence looks confusing; it is putting the
                    //                 // address of the element in the global var, into which the value
                    //                 // will be placed, into the indirection register, and when the
                    //                 // store of the value occurs, the value will "pass through"
                    //                 // the address in the indirection register to end up in the global
                    //                 // address space.
                    //                 self.instructions.push(ArmIns::Load {
                    //                     src: format!(".{}_idx{}", ident, idx),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 idx += 1;
                    //                 self.instructions.push(ArmIns::Store {
                    //                     src: value_reg.clone(),
                    //                     dst: indirection_reg.clone()
                    //                 });
                    //                 self.free_register(value_reg);
                    //                 self.free_register(indirection_reg);
                    //             }
                    //             Type::Double =>
                    //                 panic!("TODO: Load global MSW and LSW offsets into value registers"),
                    //             Type::Array(_) =>
                    //                 panic!("TODO: Load global array offsets into value registers")
                    //         }
                    //     }
                    // };
                }
                out
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
