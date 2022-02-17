use parser;

use itertools::EitherOrBoth::{Both, Left, Right};
use itertools::Itertools;
use linked_hash_set::LinkedHashSet;
use parser::AstNode;
use std::collections::HashMap;

use ieee754::Ieee754;
use std::fs::File;
use std::io::Write;

use super::instructions::ArmIns;
use super::ir::IRNode;
use super::memory::Pointer;
use super::values::{DoubleValue, IntegerValue, TypeFlag, TypedValue};
use backend::arm::registers::CoreRegister;
use parser::{DyadicVerb, MonadicVerb};
use std::ops::Deref;

pub struct GlobalContext {
    ident_map: HashMap<String, Vec<Box<dyn TypedValue>>>,
    heap_pointer: i32,
    heap_size: i32,
    double_constant_pool_label: &'static str,
    double_constant_pool_offsets: HashMap<String, i32>, // map of stringified values to offsets
    double_constant_pool_words: Vec<u32>,
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalContext {
    pub fn new() -> GlobalContext {
        GlobalContext {
            ident_map: HashMap::new(),
            heap_size: 128,
            heap_pointer: 0,
            double_constant_pool_label: ".Ldbl_pool",
            double_constant_pool_offsets: HashMap::new(),
            double_constant_pool_words: vec![],
        }
    }

    pub fn write_preamble_to_file(&self, assembly_file: &mut File) {
        let mut preamble = vec![];
        let mut subbed = 0;
        // The FP should start out where the SP is.
        preamble.push(ArmIns::Move {
            src: CoreRegister::SP,
            dst: CoreRegister::FP,
        });
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
                imm: to_sub,
            });
            subbed += to_sub;
        }
        // Stacks must appear after the heap, so we adjust the SP now.
        preamble.push(ArmIns::Move {
            src: CoreRegister::FP,
            dst: CoreRegister::SP,
        });
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
                imm: to_add,
            });
            added += to_add;
        }
        // Reset SP as well
        postamble.push(ArmIns::Move {
            src: CoreRegister::FP,
            dst: CoreRegister::SP,
        });
        for instr in postamble {
            writeln!(assembly_file, "{}", instr).expect("write failure");
        }
    }

    pub fn write_double_constant_pool_to_file(&self, assembly_file: &mut File) {
        writeln!(assembly_file, "{}:", self.double_constant_pool_label).expect("write failure");
        for value in self.double_constant_pool_words.iter() {
            writeln!(assembly_file, "\t.word\t{}", value).expect("write failure");
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

    pub fn heap_allocate_int(&mut self) -> IntegerValue {
        let offset = self._heap_allocate(4);
        IntegerValue::new(Pointer::Heap(offset))
    }

    pub fn heap_allocate_double(&mut self) -> DoubleValue {
        let offset = self._heap_allocate(8);
        DoubleValue::new(Pointer::Heap(offset))
    }

    pub fn set_ident_values(&mut self, ident: &str, values: &[Box<dyn TypedValue>]) {
        for value in values {
            // Caller is responsible for migrating values to Heap first if necessary; enforce that here.
            match value.is_entirely_on_heap() {
                true => {} // OK
                false => {
                    panic!("TypedValue {:?} is not entirely on the heap; caller was responsible for ensuring this.", value.deref());
                }
            }
        }

        // TODO: There could be previous values in the heap that we lose by overwriting
        // here. We should implement refcounting so that we can deallocate those that will
        // no longer be referred to after this insert.
        self.ident_map.insert(ident.to_string(), values.to_vec());
    }

    pub fn get_ident_values(&mut self, ident: &str) -> Vec<Box<dyn TypedValue>> {
        self.ident_map.get(ident).unwrap().clone()
    }

    pub fn get_double_constant_pool_offset(&mut self, value: f64) -> (&'static str, i32) {
        let offset = match self.double_constant_pool_offsets.get(&value.to_string()) {
            Some(offset) => *offset,
            None => {
                let bits = value.bits();
                let hex_rep = format!("{:x}", bits);
                let binary_rep = format!("{:064b}", bits);
                println!(
                    "IEEE754 double hex representation of {} is: hex={}, binary={}",
                    value, hex_rep, binary_rep
                );
                let lsw: u32 = (bits & 0xFFFFFFFF) as u32;
                println!("Decimal LSW of {}: {}", value, lsw);
                let msw: u32 = ((bits >> 32) & 0xFFFFFFFF) as u32;
                println!("Decimal MSW of {}: {}", value, msw);
                let offset = (self.double_constant_pool_words.len() * 4) as i32;
                self.double_constant_pool_words.push(lsw);
                self.double_constant_pool_words.push(msw);
                self.double_constant_pool_offsets
                    .insert(value.to_string(), offset);
                offset
            }
        };
        (self.double_constant_pool_label, offset)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    stack_pointer: i32,
    stack_size: i32,
    instructions: Vec<ArmIns>,
    available_registers: LinkedHashSet<CoreRegister>,
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self::new()
    }
}

impl BasicBlock {
    pub fn new() -> BasicBlock {
        let stack_size = 512; // TODO: temporary default
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
                imm: to_sub,
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
                CoreRegister::R3,
                CoreRegister::R4,
                CoreRegister::R5,
                CoreRegister::R6,
            ]
            .iter()
            .cloned()
            .collect(),
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
                imm: to_add,
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

    pub fn stack_allocate_int(&mut self) -> IntegerValue {
        let offset = self.stack_allocate_width(4);
        IntegerValue::new(Pointer::Stack(offset))
    }

    pub fn stack_allocate_double(&mut self) -> DoubleValue {
        let offset = self.stack_allocate_width(8);
        DoubleValue::new(Pointer::Stack(offset))
    }

    pub fn claim_register(&mut self) -> CoreRegister {
        match self.available_registers.pop_front() {
            Some(r) => r,
            None => panic!("No register is available to claim."),
        }
    }

    pub fn free_register(&mut self, reg: CoreRegister) {
        if self.available_registers.contains(&reg) {
            panic!("Attempted to free register {} but it is not claimed.", reg)
        }
        self.available_registers.insert(reg);
    }

    pub fn ir(
        &mut self,
        instruction: IRNode,
        globalctx: &mut GlobalContext,
    ) -> Vec<Box<dyn TypedValue>> {
        match instruction {
            IRNode::PushIntegerOntoStack(imm) => {
                let val = self.stack_allocate_int();
                val.set_value(imm, self);
                vec![Box::new(val)]
            }
            IRNode::PushDoublePrecisionFloatOntoStack(num) => {
                let val = self.stack_allocate_double();
                val.set_value(num, self, globalctx);
                vec![Box::new(val)]
            }
            IRNode::ApplyMonadicVerbToTypedValue(verb, value) => {
                match verb {
                    MonadicVerb::Increment => {
                        value.increment(self);
                        vec![value] // we've updated the existing value in-place
                    }
                    MonadicVerb::Square => {
                        value.square(self);
                        vec![value] // we've updated the existing value in-place
                    }
                    MonadicVerb::Negate => {
                        value.negate(self);
                        vec![value] // we've updated the existing value in-place
                    }
                    MonadicVerb::Ceiling => {
                        vec![value.ceiling(self)] // ceil(double) allocates a new int return value
                    }
                    MonadicVerb::Reciprocal => {
                        vec![value.reciprocal(self, globalctx)] // ceil(double) allocates a new int return value
                    }
                    _ => unimplemented!("TODO: Support monadic verb: {:?}", verb),
                }
            }
            IRNode::ApplyDyadicVerbToTypedValues { verb, lhs, rhs } => match verb {
                DyadicVerb::Plus => vec![lhs.sum(rhs, self)],
                DyadicVerb::Times => vec![lhs.product(rhs, self)],
                DyadicVerb::Minus => vec![lhs.difference(rhs, self)],
                DyadicVerb::LessThan => vec![lhs.compare_lt(rhs, self)],
                DyadicVerb::Equal => vec![lhs.compare_eq(rhs, self)],
                DyadicVerb::LargerThan => vec![lhs.compare_gt(rhs, self)],
                DyadicVerb::Divide => vec![lhs.quotient(rhs, self)],
                _ => unimplemented!(
                    "TODO: Support dyadic verb {:?} on values lhs={:?}, rhs={:?}",
                    verb,
                    lhs,
                    rhs
                ),
            },
            IRNode::ReduceTypedValues(verb, values) => {
                // Initialize the accumulator to the last value
                let accum_reg = self.claim_register();
                let accum_value = values.last().unwrap();
                match &accum_value.type_flag() {
                    TypeFlag::Integer => {
                        let accum_int: &IntegerValue = accum_value
                            .as_any()
                            .downcast_ref::<IntegerValue>()
                            .expect("an IntegerValue");
                        accum_int.get_value(accum_reg, self)
                    }
                    _ => unimplemented!(
                        "TODO: Support initial accumulation value: {:?}",
                        accum_value
                    ),
                }

                // Accumulate from right to left.
                let operand_reg = self.claim_register();
                for value in values[0..values.len() - 1].iter().rev() {
                    match &value.type_flag() {
                        TypeFlag::Integer => {
                            let value_int: &IntegerValue = value
                                .as_any()
                                .downcast_ref::<IntegerValue>()
                                .expect("an IntegerValue");
                            value_int.get_value(operand_reg, self);
                        }
                        _ => unimplemented!("TODO: Support accumulation operand: {:?}", value),
                    }
                    match verb {
                        DyadicVerb::Plus => self.push(ArmIns::Add {
                            dst: accum_reg,
                            src: operand_reg,
                            add: accum_reg,
                        }),
                        DyadicVerb::Minus => self.push(ArmIns::Sub {
                            dst: accum_reg,
                            src: operand_reg,
                            sub: accum_reg,
                        }),
                        DyadicVerb::Times => self.push(ArmIns::Multiply {
                            dst: accum_reg,
                            src: operand_reg,
                            mul: accum_reg,
                        }),
                        _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb),
                    }
                }
                self.free_register(operand_reg);

                let out_value = self.stack_allocate_int();
                out_value.set_value_from_register(accum_reg, self);
                self.free_register(accum_reg);
                // TODO: we should decrement refcounts of all input values before returning
                vec![Box::new(out_value)]
            }
            IRNode::AssignTypedValuesToGlobal { ident: _, values } => {
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
    expr: &AstNode,
) -> Vec<Box<dyn TypedValue>> {
    match expr {
        parser::AstNode::Integer(int) => bb.ir(IRNode::PushIntegerOntoStack(*int), globalctx),
        parser::AstNode::DoublePrecisionFloat(num) => {
            bb.ir(IRNode::PushDoublePrecisionFloatOntoStack(*num), globalctx)
        }
        parser::AstNode::Terms(terms) => {
            let mut values = vec![];
            for term in terms {
                values.extend(compile_expr(globalctx, bb, term));
            }
            values
        }
        parser::AstNode::MonadicOp { verb, expr } => {
            let vals = compile_expr(globalctx, bb, expr);
            let mut out = vec![];
            for val in &vals {
                out.extend(bb.ir(
                    IRNode::ApplyMonadicVerbToTypedValue(*verb, val.clone()),
                    globalctx,
                ));
            }
            out // this should always be the same as vals because we updated in-place on the stack
        }
        parser::AstNode::DyadicOp { verb, lhs, rhs } => {
            let rhs_values = compile_expr(globalctx, bb, rhs);
            let lhs_values = compile_expr(globalctx, bb, lhs);
            if rhs_values.len() != lhs_values.len()
                && (lhs_values.len() != 1 && rhs_values.len() != 1)
            {
                panic!(
                    "Dyadic op lhs has length {}, rhs has length {}; don't know how to proceed.",
                    lhs_values.len(),
                    rhs_values.len()
                )
            }

            // If the LHS and RHS are different lengths, the shorter of the two is repeated to the length of the other.
            let repeated_value = match lhs_values.len() {
                1 => lhs_values.get(0).unwrap(),
                _ => rhs_values.get(0).unwrap(),
            };

            let mut dest_values = vec![];
            for pair in lhs_values.iter().zip_longest(rhs_values.iter()) {
                let (l, r) = match pair {
                    Both(l, r) => (l, r),
                    Left(l) => (l, repeated_value),
                    Right(r) => (repeated_value, r),
                };
                dest_values.extend(bb.ir(
                    IRNode::ApplyDyadicVerbToTypedValues {
                        verb: *verb,
                        lhs: l.clone(),
                        rhs: r.clone(),
                    },
                    globalctx,
                ));
            }
            dest_values
        }
        parser::AstNode::Reduce { verb, expr } => {
            let values = compile_expr(globalctx, bb, expr);
            bb.ir(IRNode::ReduceTypedValues(*verb, values), globalctx)
        }
        parser::AstNode::GlobalVarAssgmt { ident, expr } => {
            let values = compile_expr(globalctx, bb, expr);
            let heap_values = bb.ir(
                IRNode::AssignTypedValuesToGlobal {
                    ident: ident.clone(),
                    values,
                },
                globalctx,
            );
            globalctx.set_ident_values(ident, &heap_values);
            heap_values
        }
        parser::AstNode::Ident(ident) => globalctx.get_ident_values(ident),
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}
