use super::memory::Pointer;
use super::compiler::{BasicBlock, GlobalContext};
use super::instructions::{ArmIns};
use std::any::Any;
use ieee754::Ieee754;

use std::fmt::{Formatter, Error};
use core::fmt::Debug;
use backend::arm::registers::{CoreRegister, ExtensionRegister};

#[derive(Debug, Copy, Clone)]
pub enum TypeFlag {
    Integer,
    Double
}

pub trait TypedValue: TypedValueClone + Debug {
    fn type_flag(&self) -> TypeFlag;
    fn is_entirely_on_heap(&self) -> bool; // true if all pointers are on heap, false otherwise
    fn to_string(&self) -> &str; // to_string representation for i.e. debugging
    fn as_any(&self) -> &dyn Any;
    fn persist_to_heap(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> Box<dyn TypedValue>; // may allocate

    fn print(&self, basic_block: &mut BasicBlock);
    fn increment(&self, basic_block: &mut BasicBlock); // increments in-place without allocation
    fn square(&self, basic_block: &mut BasicBlock);
    fn negate(&self, basic_block: &mut BasicBlock);
    fn ceiling(&self, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // may allocate
    fn sum(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // allocates
    fn product(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // allocates
    fn difference(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // allocates

    fn compare_lt(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // allocates
    fn compare_gt(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // allocates
    fn compare_eq(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue>; // allocates
}

pub trait TypedValueClone {
    fn clone_box(&self) -> Box<dyn TypedValue>;
}

impl<T: 'static + TypedValue + Clone> TypedValueClone for T {
    fn clone_box(&self) -> Box<dyn TypedValue> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn TypedValue> {
    fn clone(&self) -> Box<dyn TypedValue> {
        self.clone_box()
    }
}

// impl Debug for dyn TypedValue {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         write!(f, "TypedValue<{}>", self.to_string())
//     }
// }

#[derive(Debug, Clone)]
pub struct IntegerValue {
    ty: TypeFlag,
    pointer: Pointer
}

impl IntegerValue {
    pub fn new(pointer: Pointer) -> IntegerValue {
        IntegerValue {
            ty: TypeFlag::Integer,
            pointer
        }
    }

    pub fn set_value(&self, value: i32, basic_block: &mut BasicBlock) {
        let transfer_reg = basic_block.claim_register();
        basic_block.push(ArmIns::MoveImm { imm: value, dst: transfer_reg });
        self.pointer.write(transfer_reg, basic_block);
        basic_block.free_register(transfer_reg);
    }

    pub fn set_value_from_register(&self, src: CoreRegister, basic_block: &mut BasicBlock) {
        self.pointer.write(src, basic_block);
    }

    pub fn get_value(&self, dst: CoreRegister, basic_block: &mut BasicBlock) {
        self.pointer.read(dst, basic_block);
    }
}

impl TypedValue for IntegerValue {

    fn type_flag(&self) -> TypeFlag {
        self.ty
    }

    fn is_entirely_on_heap(&self) -> bool {
        match self.pointer {
            Pointer::Heap(_) => true,
            _ => false
        }
    }

    fn to_string(&self) -> &str {
        todo!()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn persist_to_heap(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> Box<dyn TypedValue> {
        match self.pointer {
            Pointer::Heap(_) => Box::new(self.clone()),  // already on the heap, nothing to do
            Pointer::Stack(_) => {
                let out = globalctx.heap_allocate_int();
                let transfer_reg = basic_block.claim_register();
                self.pointer.read(transfer_reg, basic_block);
                out.set_value_from_register(transfer_reg, basic_block);
                basic_block.free_register(transfer_reg);
                Box::new(out)
            }
        }
    }

    fn print(&self, basic_block: &mut BasicBlock) {
        self.pointer.read(CoreRegister::R0, basic_block);
        basic_block.push(ArmIns::BranchAndLink { addr: "jprint_int" });
    }

    fn increment(&self, basic_block: &mut BasicBlock) {
        let increment_reg = basic_block.claim_register();
        self.pointer.read(increment_reg, basic_block);
        basic_block.push(ArmIns::AddImm {
            dst: increment_reg,
            src: increment_reg,
            imm: 1
        });
        self.pointer.write(increment_reg, basic_block);
        basic_block.free_register(increment_reg);
    }

    fn square(&self, basic_block: &mut BasicBlock) {
        let multiply_reg = basic_block.claim_register();
        self.pointer.read(multiply_reg, basic_block);
        basic_block.push(ArmIns::Multiply {
            dst: multiply_reg,
            src: multiply_reg,
            mul: multiply_reg,
        });
        self.pointer.write(multiply_reg, basic_block);
        basic_block.free_register(multiply_reg);
    }

    fn negate(&self, basic_block: &mut BasicBlock) {
        let temp_reg = basic_block.claim_register();
        let negation_reg = basic_block.claim_register();
        self.pointer.read(temp_reg, basic_block);
        basic_block.push(ArmIns::MoveImm {
            dst: negation_reg, imm: 0 });
        // subtract the immediate from 0
        basic_block.push(ArmIns::Sub {
            dst: temp_reg,
            src: negation_reg,
            sub: temp_reg
        });
        self.pointer.write(temp_reg, basic_block);
        basic_block.free_register(temp_reg);
        basic_block.free_register(negation_reg);
    }

    fn ceiling(&self, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        Box::new(self.clone())  // nothing to do; ceil(int) is always the given int
    }

    fn sum(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.read(lhs_reg, basic_block);
                addend_int.pointer.read(rhs_reg, basic_block);
                basic_block.push(ArmIns::Add { dst: rhs_reg, src: lhs_reg, add: rhs_reg });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.write(rhs_reg, basic_block);
                basic_block.free_register(lhs_reg);
                basic_block.free_register(rhs_reg);
                Box::new(out_value)
            }
        }
    }

    fn product(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.read(lhs_reg, basic_block);
                addend_int.pointer.read(rhs_reg, basic_block);
                basic_block.push(ArmIns::Multiply { dst: rhs_reg, src: lhs_reg, mul: rhs_reg });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.write(rhs_reg, basic_block);
                basic_block.free_register(lhs_reg);
                basic_block.free_register(rhs_reg);
                Box::new(out_value)
            }
        }
    }

    fn difference(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.read(lhs_reg, basic_block);
                addend_int.pointer.read(rhs_reg, basic_block);
                basic_block.push(ArmIns::Sub { dst: rhs_reg, src: lhs_reg, sub: rhs_reg });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.write(rhs_reg, basic_block);
                basic_block.free_register(lhs_reg);
                basic_block.free_register(rhs_reg);
                Box::new(out_value)
            }
        }
    }

    fn compare_lt(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.read(lhs_reg, basic_block);
                addend_int.pointer.read(rhs_reg, basic_block);
                basic_block.push(ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                basic_block.push(ArmIns::MoveLT { dst: rhs_reg, src: 1 });
                basic_block.push(ArmIns::MoveGE { dst: rhs_reg, src: 0 });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.write(rhs_reg, basic_block);
                basic_block.free_register(lhs_reg);
                basic_block.free_register(rhs_reg);
                Box::new(out_value)
            }
        }
    }

    fn compare_gt(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.read(lhs_reg, basic_block);
                addend_int.pointer.read(rhs_reg, basic_block);
                basic_block.push(ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                basic_block.push(ArmIns::MoveGT { dst: rhs_reg, src: 1 });
                basic_block.push(ArmIns::MoveLE { dst: rhs_reg, src: 0 });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.write(rhs_reg, basic_block);
                basic_block.free_register(lhs_reg);
                basic_block.free_register(rhs_reg);
                Box::new(out_value)
            }
        }
    }

    fn compare_eq(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.read(lhs_reg, basic_block);
                addend_int.pointer.read(rhs_reg, basic_block);
                basic_block.push(ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                basic_block.push(ArmIns::MoveEQ { dst: rhs_reg, src: 1 });
                basic_block.push(ArmIns::MoveNE { dst: rhs_reg, src: 0 });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.write(rhs_reg, basic_block);
                basic_block.free_register(lhs_reg);
                basic_block.free_register(rhs_reg);
                Box::new(out_value)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct DoubleValue {
    ty: TypeFlag,
    msw: Pointer,
    lsw: Pointer
}

impl DoubleValue {
    pub fn new(msw: Pointer, lsw: Pointer) -> DoubleValue {
        DoubleValue {
            ty: TypeFlag::Double,
            msw,
            lsw
        }
    }

    pub fn set_value(&self, num: f64, basic_block: &mut BasicBlock) {
        let bits = num.bits();
        let hex_rep = format!("{:x}", bits);
        let binary_rep = format!("{:064b}", bits);
        println!("IEEE754 double hex representation of {} is: hex={}, binary={}", num, hex_rep, binary_rep);
        // Note: due to limited width of ARM's MOV immediate field,
        // we split the initialization of both MSW and LWS over multiple
        // per-byte instructions; probably could be optimized.
        { // LSW
            let lsw_reg = basic_block.claim_register();
            {
                let half1 = (bits & 0xFFFF) as u16;
                basic_block.push(ArmIns::MoveImmUnsigned {
                    imm: half1, dst: lsw_reg });
            }
            {
                let temp_reg = basic_block.claim_register();
                let half2 = ((bits >> 16) & 0xFFFF) as u16;
                basic_block.push(ArmIns::MoveImmUnsigned {
                    imm: half2, dst: temp_reg });
                basic_block.push(ArmIns::LeftShift {
                    src: temp_reg, dst: temp_reg, n_bits: 16 });
                basic_block.push(ArmIns::Add {
                    dst: lsw_reg, src: lsw_reg, add: temp_reg });
                basic_block.free_register(temp_reg);
            }
            self.lsw.write(lsw_reg, basic_block);
            basic_block.free_register(lsw_reg);
        }
        { // MSW
            let msw_reg = basic_block.claim_register();
            {
                let half1 = ((bits >> 32) & 0xFFFF) as u16;
                basic_block.push(ArmIns::MoveImmUnsigned {
                    imm: half1, dst: msw_reg });
            }
            {
                let temp_reg = basic_block.claim_register();
                let half2 = ((bits >> 48) & 0xFFFF) as u16;
                basic_block.push(ArmIns::MoveImmUnsigned {
                    imm: half2, dst: temp_reg });
                basic_block.push(ArmIns::LeftShift {
                    src: temp_reg, dst: temp_reg, n_bits: 16 });
                basic_block.push(ArmIns::Add {
                    dst: msw_reg, src: msw_reg, add: temp_reg });
                basic_block.free_register(temp_reg);
            }
            self.msw.write(msw_reg, basic_block);
            basic_block.free_register(msw_reg);
        }
    }
}

impl TypedValue for DoubleValue {

    fn type_flag(&self) -> TypeFlag {
        self.ty
    }

    fn is_entirely_on_heap(&self) -> bool {
        match (&self.msw, &self.lsw) {
            (Pointer::Heap(_), Pointer::Heap(_)) => true,
            _ => false
        }
    }

    fn to_string(&self) -> &str {
        todo!()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn persist_to_heap(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> Box<dyn TypedValue> {
        match (&self.msw, &self.lsw) {
            (Pointer::Heap(_), Pointer::Heap(_)) => Box::new(self.clone()), // TODO: increment refcount
            (Pointer::Heap(_), Pointer::Stack(_)) => {
                unimplemented!("TODO: persist only stack Double MSW to Heap")
            },
            (Pointer::Stack(_), Pointer::Heap(_)) => {
                unimplemented!("TODO: persist only stack Double LSW to Heap")
            }
            (Pointer::Stack(_), Pointer::Stack(_)) => {
                let out = globalctx.heap_allocate_double();
                let transfer_reg = basic_block.claim_register();
                self.msw.read(transfer_reg, basic_block);
                out.msw.write(transfer_reg, basic_block);
                self.lsw.read(transfer_reg, basic_block);
                out.lsw.write(transfer_reg, basic_block);
                basic_block.free_register(transfer_reg);
                Box::new(out)
            }
        }
    }

    fn print(&self, basic_block: &mut BasicBlock) {
        // TODO: reclaim this stack entry after the call to print, we only use it to load register d0
        let sp_offset = basic_block.stack_allocate_width(8);
        self.msw.copy_to_stack_offset(sp_offset + 4, basic_block);
        self.lsw.copy_to_stack_offset(sp_offset, basic_block);
        basic_block.push(ArmIns::LoadExtensionRegisterWidth64 {
            dst: ExtensionRegister::D0, src: CoreRegister::SP, offsets: vec![sp_offset] });
        basic_block.push(ArmIns::BranchAndLink { addr: "jprint_double" });
    }

    fn increment(&self, _basic_block: &mut BasicBlock) {
        unimplemented!("TODO: implement DoubleValue::increment")
    }

    fn square(&self, _basic_block: &mut BasicBlock) {
        unimplemented!("TODO: implement DoubleValue::square")
    }

    fn negate(&self, basic_block: &mut BasicBlock) {
        let msw_reg = basic_block.claim_register();
        self.msw.read(msw_reg, basic_block);
        basic_block.push(ArmIns::ExclusiveOr {
            dst: msw_reg,
            src: msw_reg,
            operand: 0x80000000
        }); // flip the MSB of the MSW (2's complement sign bit)
        self.msw.write(msw_reg, basic_block);
        basic_block.free_register(msw_reg);
    }

    fn ceiling(&self, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        // TODO: reclaim this stack entry after the call to print, we only use it to load register d0
        let sp_offset = basic_block.stack_allocate_width(8);
        self.msw.copy_to_stack_offset(sp_offset + 4, basic_block);
        self.lsw.copy_to_stack_offset(sp_offset, basic_block);
        basic_block.push(ArmIns::LoadExtensionRegisterWidth64 {
            dst: ExtensionRegister::D0, src: CoreRegister::SP, offsets: vec![sp_offset] });
        basic_block.push(ArmIns::BranchAndLink { addr: "jceiling" });
        let new_value = basic_block.stack_allocate_int();
        new_value.set_value_from_register(CoreRegister::R0, basic_block);
        // TODO: we should decrement the old value's refcount so we can reuse its space later on.
        Box::new(new_value)
    }

    fn sum(&self, _addend: Box<dyn TypedValue>, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        unimplemented!("TODO: implement DoubleValue::sum")
    }

    fn product(&self, _addend: Box<dyn TypedValue>, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        todo!()
    }

    fn difference(&self, _addend: Box<dyn TypedValue>, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        todo!()
    }

    fn compare_lt(&self, _addend: Box<dyn TypedValue>, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        todo!()
    }

    fn compare_gt(&self, _addend: Box<dyn TypedValue>, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        todo!()
    }

    fn compare_eq(&self, _addend: Box<dyn TypedValue>, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        todo!()
    }
}

impl std::fmt::Display for IntegerValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_fmt(format_args!("IntegerValue<pointer={}>", self.pointer))
    }
}

impl std::fmt::Display for DoubleValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_fmt(format_args!("DoubleValue<msw={}, lsw={}>", self.msw, self.lsw))
    }
}
