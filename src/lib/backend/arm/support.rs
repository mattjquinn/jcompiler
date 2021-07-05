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
                    Pointer::Heap(_) => self.clone(),
                    Pointer::Stack(_) => {
                        let out = globalctx.heap_allocate_int();
                        match &out {
                            TypedValue::Integer(dst) => {
                                let transfer_reg = basic_block.claim_register();
                                src.load(transfer_reg.clone(), basic_block);
                                dst.store(transfer_reg.clone(), basic_block);
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
                        panic!("TODO: persist stack Double MSW to Heap")
                    },
                    (Pointer::Stack(_), Pointer::Heap(_)) => {
                        panic!("TODO: persist stack Double LSW to Heap")
                    }
                    (Pointer::Stack(_), Pointer::Stack(_)) => {
                        panic!("TODO: persist stack Double both MSW and LSW to Heap")
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
                panic!("TODO: handle stack allocation for Double")
                // let bits = num.bits();
                // let hex_rep = format!("{:x}", bits);
                // let binary_rep = format!("{:064b}", bits);
                // println!("IEEE754 double hex representation of {} is: hex={}, binary={}", num, hex_rep, binary_rep);
                //
                // let offset = self.stack_allocate_double();
                //
                // // Note: due to limited width of ARM's MOV immediate field,
                // // we split the initialization of both MSW and LWS over multiple
                // // per-byte instructions; probably could be optimized.
                // { // LSW
                //     let lsw_reg = self.claim_register();
                //     {
                //         let half1 = (bits & 0xFFFF) as u16;
                //         self.instructions.push(ArmIns::MoveImmUnsigned {
                //             imm: half1, dst: lsw_reg.clone() });
                //     }
                //     {
                //         let temp_reg = self.claim_register();
                //         let half2 = ((bits >> 16) & 0xFFFF) as u16;
                //         self.instructions.push(ArmIns::MoveImmUnsigned {
                //             imm: half2, dst: temp_reg.clone() });
                //         self.instructions.push(ArmIns::LeftShift {
                //             src: temp_reg.clone(), dst: temp_reg.clone(), n_bits: 16 });
                //         self.instructions.push(ArmIns::Add {
                //             dst: lsw_reg.clone(), src: lsw_reg.clone(), add: temp_reg.clone() });
                //         self.free_register(temp_reg);
                //     }
                //     self.instructions.push(ArmIns::StoreOffset {
                //         dst: ArmRegister::FP, src: lsw_reg.clone(),
                //         offsets: vec![offset + 4]});// TODO: if we have a DoubleOffset(msw,lsw) we won't need the manual + 4
                //     self.free_register(lsw_reg);
                // }
                // { // MSW
                //     let msw_reg = self.claim_register();
                //     {
                //         let half1 = ((bits >> 32) & 0xFFFF) as u16;
                //         self.instructions.push(ArmIns::MoveImmUnsigned {
                //             imm: half1, dst: msw_reg.clone() });
                //     }
                //     {
                //         let temp_reg = self.claim_register();
                //         let half2 = ((bits >> 48) & 0xFFFF) as u16;
                //         self.instructions.push(ArmIns::MoveImmUnsigned {
                //             imm: half2, dst: temp_reg.clone() });
                //         self.instructions.push(ArmIns::LeftShift {
                //             src: temp_reg.clone(), dst: temp_reg.clone(), n_bits: 16 });
                //         self.instructions.push(ArmIns::Add {
                //             dst: msw_reg.clone(), src: msw_reg.clone(), add: temp_reg.clone() });
                //         self.free_register(temp_reg);
                //     }
                //     self.instructions.push(ArmIns::StoreOffset {
                //         dst: ArmRegister::FP, src: msw_reg.clone(),
                //         offsets: vec![offset]});
                //     self.free_register(msw_reg);
                // }
                // vec![TypedValue::Double{msw: Pointer::Stack(offset), lsw: Pointer::Stack(offset+4)}]
            }
            IRNode::ApplyMonadicVerbToTypedValue(verb, offset) => {
                panic!("TODO: Support ApplyMonadicVerbToTypedValue");
                // let (offset_type, offset_idx) = match &offset {
                //     Pointer::Stack(idx) => (idx.clone()),
                //     Pointer::Heap(ident) => {
                //         // If this is a global offset, we need to copy it into the heap.
                //         match &ty {
                //             Type::Integer => {
                //                 let temp_reg = self.claim_register();
                //                 let heap_offset = self.heap_allocate_int();
                //                 self.push(ArmIns::Load {
                //                     dst: temp_reg.clone(),
                //                     src: format!(".{}", ident).to_string()
                //                 });
                //                 self.push(ArmIns::Load {
                //                     dst: temp_reg.clone(),
                //                     src: format!("[{}]", temp_reg.clone())
                //                 });
                //                 self.push(ArmIns::StoreOffset {
                //                     src: temp_reg.clone(),
                //                     dst: ArmRegister::FP,
                //                     offsets: vec![heap_offset]
                //                 });
                //                 self.free_register(temp_reg);
                //                 (Type::Integer, heap_offset)
                //             },
                //             Type::Double => {
                //                 let temp_reg = self.claim_register();
                //                 let heap_offset = self.heap_allocate_double();
                //                 self.push(ArmIns::Load {
                //                     dst: temp_reg.clone(),
                //                     src: format!(".{}_msw", ident).to_string()
                //                 });
                //                 self.push(ArmIns::Load {
                //                     dst: temp_reg.clone(),
                //                     src: format!("[{}]", temp_reg.clone())
                //                 });
                //                 self.push(ArmIns::StoreOffset {
                //                     src: temp_reg.clone(),
                //                     dst: ArmRegister::FP,
                //                     offsets: vec![heap_offset]
                //                 });
                //                 self.push(ArmIns::Load {
                //                     dst: temp_reg.clone(),
                //                     src: format!(".{}_lsw", ident).to_string()
                //                 });
                //                 self.push(ArmIns::Load {
                //                     dst: temp_reg.clone(),
                //                     src: format!("[{}]", temp_reg.clone())
                //                 });
                //                 self.push(ArmIns::StoreOffset {
                //                     src: temp_reg.clone(),
                //                     dst: ArmRegister::FP,
                //                     offsets: vec![heap_offset + 4]
                //                 });
                //                 self.free_register(temp_reg);
                //                 (Type::Integer, heap_offset)
                //             },
                //             _ => panic!("TODO: allocate heap space for type: {}", ty)
                //         }
                //     }
                // };
                // let new_offset = match verb {
                //     MonadicVerb::Increment => {
                //         if offset_type != Type::Integer {
                //             panic!("TODO: Support monadic increment of type: {}", offset_type)
                //         }
                //         let temp_reg = self.claim_register();
                //         self.instructions.push(ArmIns::LoadOffset {
                //             dst: temp_reg.clone(),
                //             src: ArmRegister::FP,
                //             offsets: vec![offset_idx]
                //         });
                //         self.instructions.push(ArmIns::AddImm {
                //             dst: temp_reg.clone(),
                //             src: temp_reg.clone(),
                //             imm: 1
                //         });
                //         self.instructions.push(ArmIns::StoreOffset {
                //             src: temp_reg.clone(),
                //             dst: ArmRegister::FP,
                //             offsets: vec![offset_idx]
                //         });
                //         self.free_register(temp_reg);
                //         None   // we've updated the existing offset in-place on the stack
                //     },
                //     MonadicVerb::Square => {
                //         if offset_type != Type::Integer {
                //             panic!("TODO: Support monadic square of type: {}", offset_type)
                //         }
                //         let temp_reg = self.claim_register();
                //         self.instructions.push(ArmIns::LoadOffset {
                //             dst: temp_reg.clone(),
                //             src: ArmRegister::FP,
                //             offsets: vec![offset_idx]
                //         });
                //         self.instructions.push(ArmIns::Multiply {
                //             dst: temp_reg.clone(),
                //             src: temp_reg.clone(),
                //             mul: temp_reg.clone()
                //         });
                //         self.instructions.push(ArmIns::StoreOffset {
                //             src: temp_reg.clone(),
                //             dst: ArmRegister::FP,
                //             offsets: vec![offset_idx]
                //         });
                //         self.free_register(temp_reg);
                //         None   // we've updated the existing offset in-place on the stack
                //     },
                //     MonadicVerb::Negate => {
                //         match offset_type {
                //             Type::Integer => {
                //                 let temp_reg = self.claim_register();
                //                 let negation_reg = self.claim_register();
                //                 self.instructions.push(ArmIns::LoadOffset {
                //                     dst: temp_reg.clone(),
                //                     src: ArmRegister::FP,
                //                     offsets: vec![offset_idx]
                //                 });
                //                 self.instructions.push(ArmIns::MoveImm { dst: negation_reg.clone(), imm: 0 });
                //                 // subtract the immediate from 0
                //                 self.instructions.push(ArmIns::Sub {
                //                     dst: temp_reg.clone(),
                //                     src: negation_reg.clone(),
                //                     sub: temp_reg.clone()
                //                 });
                //                 self.instructions.push(ArmIns::StoreOffset {
                //                     src: temp_reg.clone(),
                //                     dst: ArmRegister::FP,
                //                     offsets: vec![offset_idx]
                //                 });
                //                 self.free_register(temp_reg);
                //                 self.free_register(negation_reg);
                //                 None   // we've updated the existing offset in-place on the stack
                //             },
                //             Type::Double => {
                //                 let msw_reg = self.claim_register();
                //                 self.instructions.push(ArmIns::LoadOffset {
                //                     dst: msw_reg.clone(),
                //                     src: ArmRegister::FP,
                //                     offsets: vec![offset_idx]
                //                 });
                //                 self.instructions.push(ArmIns::ExclusiveOr {
                //                     dst: msw_reg.clone(),
                //                     src: msw_reg.clone(),
                //                     operand: 0x80000000
                //                 }); // flip the MSB of the MSW (2's complement sign bit)
                //                 self.instructions.push(ArmIns::StoreOffset {
                //                     src: msw_reg.clone(),
                //                     dst: ArmRegister::FP,
                //                     offsets: vec![offset_idx]
                //                 });
                //                 self.free_register(msw_reg);
                //                 None   // we've updated the existing offset in-place on the stack
                //             }
                //             _ => panic!("TODO: support monadic negation for type: {}", offset_type)
                //         }
                //     },
                //     MonadicVerb::Ceiling => {
                //         match offset_type {
                //             Type::Integer => None, // nothing to do
                //             Type::Double => {
                //                 // We could call "modf" in math.h, but we don't actually need the fractional part.
                //                 // the MSW is expected in r1
                //                 self.push(ArmIns::LoadOffset {
                //                     dst: ArmRegister::R1,
                //                     src: ArmRegister::FP,
                //                     offsets: vec![offset_idx]
                //                 });
                //                 // the LSW is expected in r0
                //                 self.push(ArmIns::LoadOffset {
                //                     dst: ArmRegister::R0,
                //                     src: ArmRegister::FP,
                //                     offsets: vec![offset_idx + 4]  // if we had DoubleOffset(msw, lsw) we wouldn't need the manual + 4 here
                //                 });
                //                 self.push(ArmIns::BranchAndLink {
                //                     addr: "jcompiler_ceiling"
                //                 });
                //                 let out_offset_idx = self.heap_allocate_int();
                //                 self.instructions.push(ArmIns::StoreOffset {
                //                     src: ArmRegister::R0,
                //                     dst: ArmRegister::FP,
                //                     offsets: vec![out_offset_idx]
                //                 });
                //                 // Note: by returning a new offset here without reclaiming
                //                 // the original Double offset, we are introducing memory
                //                 // fragmentation, albeit minor.
                //                 Some(Pointer::Stack(Type::Integer, out_offset_idx))
                //             }
                //             _ => panic!("TODO: support monadic ceiling for type: {}", offset_type)
                //         }
                //     }
                //     _ => unimplemented!("TODO: Support monadic verb: {:?}", verb)
                // };
                // // TODO: Much needs to happen here.
                // // If we are given a Global offset, we need to write back to it
                // // before returning. The code to load from globals is above; we should
                // // move it to the Offset impl so that we can reuse it. I think the interface
                // // should be opaque to pass-through writes, meaning this code should only
                // // concern itself with registers, not having to do all the loading/storing
                // // from offsets. A global Offset should be able to be read/written to/from a
                // // register without needing to allocate intermediate stack/heap space. Those
                // // instructions shouldn't need to be generated here.
                // //
                // // The tricky part will getting rid of the Some() return value above. We
                // // should only ever return the Offset we're given in this monadic apply routine,
                // // but in some cases we need to swap out the type/offset for a different one.
                // // We probably need to start operating over the concept of a JValue, which may
                // // be backed by either a Global offset or a Stack/Heap offset. The interface
                // // should concern itself with read/writing to/from registers only, and everything
                // // else should be taken care of behind the scenes. Stack/Heap offsets should
                // // become invalid when their parent BasicBlock goes out of scope.
                // //
                // // When done: uncomment the NB. lines in ctest_monadic_ceiling
                // match new_offset {
                //     None => vec![offset],
                //     Some(new) => vec![new]
                // }
            },
            IRNode::ApplyDyadicVerbToTypedValues {verb, lhs, rhs} => {
                panic!("TODO: Support ApplyDyadicVerbToTypedValues");
                // let lhs_reg = self.claim_register();
                // let rhs_reg = self.claim_register();
                //
                // let l_type = match lhs {
                //     Pointer::Stack(ty, i) => {
                //         self.instructions.push(
                //             ArmIns::LoadOffset { dst: lhs_reg.clone(), src: ArmRegister::FP, offsets: vec![i] });
                //         ty
                //     },
                //     Pointer::Heap(ty, ident) => {
                //         self.instructions.push(
                //             // TODO: make this be ArmIns::LoadIdentifierAddress
                //             ArmIns::Load { dst: lhs_reg.clone(), src: format!(".{}", ident).to_string() });
                //         self.instructions.push(
                //             // TODO: make this be ArmIns::LoadFromDereferencedAddressInRegister
                //             ArmIns::Load { dst: lhs_reg.clone(), src: format!("[{}]", lhs_reg).to_string() });
                //         ty
                //     }
                // };
                // let r_type = match rhs {
                //     Pointer::Stack(ty, i) => {
                //         self.instructions.push(
                //             ArmIns::LoadOffset { dst: rhs_reg.clone(), src: ArmRegister::FP, offsets: vec![i] });
                //         ty
                //     }
                //     // TODO: it's not clear why this is unimplemented when we seem to implement it for the left-hand side above
                //     Pointer::Heap(_type, _ident) => unimplemented!("TODO: Support load from global.")
                // };
                // let unified_type = unify_types(&l_type, &r_type);
                // match verb {
                //     DyadicVerb::Plus =>
                //         self.instructions.push(
                //             ArmIns::Add { dst: rhs_reg.clone(), src: lhs_reg.clone(), add: rhs_reg.clone() }),
                //     DyadicVerb::Times =>
                //         self.instructions.push(
                //             ArmIns::Multiply { dst: rhs_reg.clone(), src: lhs_reg.clone(), mul: rhs_reg.clone() }),
                //     DyadicVerb::Minus =>
                //         self.instructions.push(
                //             ArmIns::Sub { dst: rhs_reg.clone(), src: lhs_reg.clone(), sub: rhs_reg.clone() }),
                //     DyadicVerb::LessThan => {
                //         self.instructions.push(
                //             ArmIns::Compare { lhs: lhs_reg.clone(), rhs: rhs_reg.clone() });
                //         self.instructions.push(
                //             ArmIns::MoveLT { dst: rhs_reg.clone(), src: 1 });
                //         self.instructions.push(
                //             ArmIns::MoveGE { dst: rhs_reg.clone(), src: 0 });
                //     },
                //     DyadicVerb::Equal => {
                //         self.instructions.push(
                //             ArmIns::Compare { lhs: lhs_reg.clone(), rhs: rhs_reg.clone() });
                //         self.instructions.push(
                //             ArmIns::MoveEQ { dst: rhs_reg.clone(), src: 1 });
                //         self.instructions.push(
                //             ArmIns::MoveNE { dst: rhs_reg.clone(), src: 0 });
                //     }
                //     DyadicVerb::LargerThan => {
                //         self.instructions.push(
                //             ArmIns::Compare { lhs: lhs_reg.clone(), rhs: rhs_reg.clone() });
                //         self.instructions.push(
                //             ArmIns::MoveGT { dst: rhs_reg.clone(), src: 1 });
                //         self.instructions.push(
                //             ArmIns::MoveLE { dst: rhs_reg.clone(), src: 0 });
                //     }
                //     _ => panic!("Not ready to compile dyadic verb: {:?}", verb)
                // }
                // let dest_offset = self._stack_allocate(4);
                // self.instructions.push(ArmIns::StoreOffset {
                //     src: rhs_reg.clone(), dst: ArmRegister::FP, offsets: vec![dest_offset] });
                //
                // self.free_register(lhs_reg);
                // self.free_register(rhs_reg);
                //
                // vec![Pointer::Stack(unified_type, dest_offset)]
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
