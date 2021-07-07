use parser;

use parser::{AstNode};
use itertools::Itertools;
use itertools::EitherOrBoth::{Both, Left, Right};
use std::collections::{HashMap};
use linked_hash_set::LinkedHashSet;

use std::fmt::{Formatter, Error};
use ieee754::Ieee754;
use std::fs::File;
use std::io::Write;

use super::instructions::{ArmIns};
use super::ir::{IRNode};
use backend::arm::registers::CoreRegister;
use parser::{DyadicVerb, MonadicVerb};

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

#[derive(Debug, Clone)]
pub enum TypedValue {
    Integer(Pointer),
    Double{ msw: Pointer, lsw: Pointer },
    // Array(u16)  // length of array; no element type (can be mixed)
}

impl std::fmt::Display for TypedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            TypedValue::Integer(pointer) =>
                f.write_fmt(format_args!("TypedValue::Integer<pointer={}>", pointer)),
            TypedValue::Double{ msw, lsw } =>
                f.write_fmt(format_args!("TypedValue::Double<msw={},lsw={}>", msw, lsw))
        }
    }
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
                                src.read(transfer_reg, basic_block);
                                dst.write(transfer_reg, basic_block);
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
                                msw.read(transfer_reg, basic_block);
                                msw_heap.write(transfer_reg, basic_block);
                                lsw.read(transfer_reg, basic_block);
                                lsw_heap.write(transfer_reg, basic_block);
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
        preamble.push(ArmIns::Move { src: CoreRegister::SP, dst: CoreRegister::FP });
        // Expand heap.
        while subbed < self.heap_size {
            let mut to_sub = self.heap_size - subbed;
            // TODO: rather than iterating, we can calculate the appropriate mask to use here
            // so as to do this in a single step; same with mirror code in the cleanup function.
            if to_sub > 256 {
                to_sub = 256;
            }
            preamble.push(ArmIns::SubImm {
                dst: CoreRegister::FP,
                src: CoreRegister::FP,
                imm: to_sub
            });
            subbed += to_sub;
        }
        // Stacks must appear after the heap, so we adjust the SP now.
        preamble.push(ArmIns::Move { src: CoreRegister::FP, dst: CoreRegister::SP });
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
                dst: CoreRegister::FP,
                src: CoreRegister::FP,
                imm: to_add
            });
            added += to_add;
        }
        // Reset SP as well
        postamble.push(ArmIns::Move { src: CoreRegister::FP, dst: CoreRegister::SP });
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
    available_registers: LinkedHashSet<CoreRegister>
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
                dst: CoreRegister::SP,
                src: CoreRegister::SP,
                imm: to_sub
            });
            subbed += to_sub;
        }
        BasicBlock {
            stack_pointer: 0,
            stack_size,
            instructions,
            available_registers: [
                CoreRegister::R0,
                CoreRegister::R1,
                CoreRegister::R2,
                CoreRegister::R3
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
                dst: CoreRegister::SP,
                src: CoreRegister::SP,
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

    pub fn stack_allocate_width(&mut self, width: i32) -> i32 {
        if self.stack_pointer + width > self.stack_size {
            panic!("Stack allocation failed; adding width {} will overflow stack size of {}; stack pointer is {}", width, self.stack_size, self.stack_pointer);
        }
        let out = self.stack_pointer;
        self.stack_pointer += width;
        out
    }

    fn stack_allocate_int(&mut self) -> TypedValue {
        let offset = self.stack_allocate_width(4);
        TypedValue::Integer(Pointer::Stack(offset))
    }

    pub fn stack_allocate_double(&mut self) -> TypedValue {
        let msw = Pointer::Stack(self.stack_allocate_width(4));
        let lsw = Pointer::Stack(self.stack_allocate_width(4));
        TypedValue::Double{msw, lsw}
    }

    pub fn claim_register(&mut self) -> CoreRegister {
        match self.available_registers.pop_front() {
            Some(r) => r,
            None => panic!("No register is available to claim.")
        }
    }

    pub fn free_register(&mut self, reg: CoreRegister) {
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
                        self.push(ArmIns::MoveImm { imm, dst: transfer_reg });
                        dst.write(transfer_reg, self);
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
                                    imm: half1, dst: lsw_reg });
                            }
                            {
                                let temp_reg = self.claim_register();
                                let half2 = ((bits >> 16) & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half2, dst: temp_reg });
                                self.push(ArmIns::LeftShift {
                                    src: temp_reg, dst: temp_reg, n_bits: 16 });
                                self.push(ArmIns::Add {
                                    dst: lsw_reg, src: lsw_reg, add: temp_reg });
                                self.free_register(temp_reg);
                            }
                            lsw.write(lsw_reg, self);
                            self.free_register(lsw_reg);
                        }
                        { // MSW
                            let msw_reg = self.claim_register();
                            {
                                let half1 = ((bits >> 32) & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half1, dst: msw_reg });
                            }
                            {
                                let temp_reg = self.claim_register();
                                let half2 = ((bits >> 48) & 0xFFFF) as u16;
                                self.push(ArmIns::MoveImmUnsigned {
                                    imm: half2, dst: temp_reg });
                                self.push(ArmIns::LeftShift {
                                    src: temp_reg, dst: temp_reg, n_bits: 16 });
                                self.push(ArmIns::Add {
                                    dst: msw_reg, src: msw_reg, add: temp_reg });
                                self.free_register(temp_reg);
                            }
                            msw.write(msw_reg, self);
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
                                pointer.read(increment_reg, self);
                                self.push(ArmIns::AddImm {
                                    dst: increment_reg,
                                    src: increment_reg,
                                    imm: 1
                                });
                                pointer.write(increment_reg, self);
                                self.free_register(increment_reg);
                                vec![value]   // we've updated the existing value in-place
                            },
                            TypedValue::Double{msw: _, lsw: _} =>
                                panic!("TODO: Support monadic increment of typed value: {:?}", value)
                        }
                    },
                    MonadicVerb::Square => {
                        match &value {
                            TypedValue::Integer(pointer) => {
                                let multiply_reg = self.claim_register();
                                pointer.read(multiply_reg, self);
                                self.instructions.push(ArmIns::Multiply {
                                    dst: multiply_reg,
                                    src: multiply_reg,
                                    mul: multiply_reg,
                                });
                                pointer.write(multiply_reg, self);
                                self.free_register(multiply_reg);
                                vec![value]   // we've updated the existing value in-place
                            },
                            TypedValue::Double{msw: _, lsw: _} =>
                                panic!("TODO: Support monadic square of typed value: {:?}", value)
                        }
                    },
                    MonadicVerb::Negate => {
                        match &value {
                            TypedValue::Integer(pointer) => {
                                let temp_reg = self.claim_register();
                                let negation_reg = self.claim_register();
                                pointer.read(temp_reg, self);
                                self.instructions.push(ArmIns::MoveImm {
                                    dst: negation_reg, imm: 0 });
                                // subtract the immediate from 0
                                self.instructions.push(ArmIns::Sub {
                                    dst: temp_reg,
                                    src: negation_reg,
                                    sub: temp_reg
                                });
                                pointer.write(temp_reg, self);
                                self.free_register(temp_reg);
                                self.free_register(negation_reg);
                                vec![value]   // we've updated the existing value in-place on the stack
                            },
                            TypedValue::Double{msw, lsw: _} => {
                                let msw_reg = self.claim_register();
                                msw.read(msw_reg, self);
                                self.instructions.push(ArmIns::ExclusiveOr {
                                    dst: msw_reg,
                                    src: msw_reg,
                                    operand: 0x80000000
                                }); // flip the MSB of the MSW (2's complement sign bit)
                                msw.write(msw_reg, self);
                                self.free_register(msw_reg);
                                vec![value]   // we've updated the existing value in-place on the stack
                            }
                        }
                    },
                    MonadicVerb::Ceiling => {
                        match &value {
                            TypedValue::Integer(_) => vec![value], // nothing to do
                            TypedValue::Double { msw, lsw } => {
                                // We could call "modf" in math.h, but we don't actually need the fractional part.
                                // TODO: we should be claiming these registers by name from the BasicBlock
                                msw.read(CoreRegister::R1, self); // the MSW is expected in r1
                                lsw.read(CoreRegister::R0, self); // the MSW is expected in r0
                                self.push(ArmIns::BranchAndLink { addr: "jcompiler_ceiling" });
                                let new_value = self.stack_allocate_int();
                                match &new_value {
                                    TypedValue::Integer(pointer) => {
                                        pointer.write(CoreRegister::R0, self);
                                    },
                                    _ => panic!("Unreachable")
                                }
                                // TODO: we should decrement the old value's refcount so
                                // we can reuse its space later on.
                                vec![new_value]
                            }
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
                        lhsptr.read(lhs_reg, self);
                        rhsptr.read(rhs_reg, self);
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
                            TypedValue::Integer(pointer) => pointer.write(rhs_reg, self),
                            _ => panic!("Unreachable")
                        }
                        self.free_register(lhs_reg);
                        self.free_register(rhs_reg);
                        vec![out_value]
                    },
                    (_, _) => panic!("TODO: Support dyadic verb on types lhs={:?}, rhs={:?}", lhs, rhs)
                }
            },
            IRNode::ReduceTypedValues(verb, values) => {
                // Initialize the accumulator to the last value
                let accum_reg = self.claim_register();
                let accum_value = values.last().unwrap();
                match &accum_value {
                    TypedValue::Integer(pointer) => pointer.read(accum_reg, self),
                    _ => panic!("TODO: Support initial accumulation value: {:?}", accum_value)
                }

                // Accumulate from right to left.
                let operand_reg = self.claim_register();
                for value in values[0..values.len()-1].iter().rev() {
                    match &value {
                        TypedValue::Integer(pointer) => pointer.read(operand_reg, self),
                        _ => panic!("TODO: Support accumulation operand: {:?}", value)
                    }
                    match verb {
                        DyadicVerb::Plus => self.push(ArmIns::Add { dst: accum_reg, src: operand_reg, add: accum_reg }),
                        DyadicVerb::Minus => self.push(ArmIns::Sub { dst: accum_reg, src: operand_reg, sub: accum_reg }),
                        DyadicVerb::Times => self.push(ArmIns::Multiply { dst: accum_reg, src: operand_reg, mul: accum_reg }),
                        _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb)
                    }
                }
                self.free_register(operand_reg);

                let out_value = self.stack_allocate_int();
                match &out_value {
                    TypedValue::Integer(pointer) => pointer.write(accum_reg, self),
                    _ => panic!("Unreachable")
                };
                self.free_register(accum_reg);
                // TODO: we should decrement refcounts of all input values before returning
                vec![out_value]
            },
            IRNode::AssignTypedValuesToGlobal {ident: _, values} => {
                let mut out = vec![];
                for value in values.iter() {
                    out.push(value.persist_to_heap(self, globalctx));
                }
                out
            }
        }
    }
}

pub fn compile_expr(
    globalctx: &mut GlobalContext,
    bb: &mut BasicBlock,
    expr: &AstNode) -> Vec<TypedValue>
{
    match expr {
        parser::AstNode::Integer(int) => {
            bb.ir(IRNode::PushIntegerOntoStack(*int), globalctx)
        },
        parser::AstNode::DoublePrecisionFloat(num) => {
            bb.ir(IRNode::PushDoublePrecisionFloatOntoStack(*num), globalctx)
        },
        parser::AstNode::Terms(terms) => {
            let mut values = vec![];
            for term in terms {
                values.extend(compile_expr(globalctx, bb, term));
            }
            values
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            let vals = compile_expr(globalctx, bb, expr);
            let mut out = vec![];
            for val in &vals {
                out.extend(bb.ir(IRNode::ApplyMonadicVerbToTypedValue(*verb, val.clone()), globalctx));
            }
            out   // this should always be the same as vals because we updated in-place on the stack
        },
        parser::AstNode::DyadicOp {verb, lhs, rhs} => {
            let rhs_values = compile_expr(globalctx, bb, rhs);
            let lhs_values = compile_expr(globalctx, bb, lhs);
            if rhs_values.len() != lhs_values.len()
                && (lhs_values.len() != 1 && rhs_values.len() != 1) {
                panic!("Dyadic op lhs has length {}, rhs has length {}; don't know how to proceed.", lhs_values.len(), rhs_values.len())
            }

            // If the LHS and RHS are different lengths, the shorter of the two is repeated to the length of the other.
            let repeated_value = match lhs_values.len() {
                1 => lhs_values.get(0).unwrap(),
                _ => rhs_values.get(0).unwrap()
            };

            let mut dest_values = vec![];
            for pair in lhs_values.iter().zip_longest(rhs_values.iter()) {
                let (l, r) = match pair {
                    Both(l, r) => (l, r),
                    Left(l) => (l, repeated_value),
                    Right(r) => (repeated_value, r)
                };
                dest_values.extend(bb.ir(IRNode::ApplyDyadicVerbToTypedValues {verb: *verb, lhs: l.clone(), rhs: r.clone()}, globalctx));
            }
            dest_values
        },
        parser::AstNode::Reduce {verb, expr} => {
            let values = compile_expr(globalctx, bb, expr);
            bb.ir(IRNode::ReduceTypedValues(*verb, values), globalctx)
        },
        parser::AstNode::GlobalVarAssgmt {ident, expr} => {
            let values = compile_expr(globalctx, bb, expr);
            let heap_values = bb.ir(IRNode::AssignTypedValuesToGlobal { ident: ident.clone(), values }, globalctx);
            globalctx.set_ident_values(ident, &heap_values);
            heap_values
        },
        parser::AstNode::Ident(ident) => globalctx.get_ident_values(ident),
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}
