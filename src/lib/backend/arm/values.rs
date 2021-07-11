use super::memory::Pointer;
use super::compiler::{BasicBlock, GlobalContext};
use super::instructions::{ArmIns};
use std::any::Any;

use std::fmt::{Formatter, Error};
use core::fmt::Debug;
use backend::arm::registers::{CoreRegister, ExtensionRegisterDoublePrecision, ExtensionRegisterSinglePrecision};

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
    fn reciprocal(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> Box<dyn TypedValue>; // may allocate
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
        self.pointer.store_width4(transfer_reg, basic_block);
        basic_block.free_register(transfer_reg);
    }

    pub fn set_value_from_register(&self, src: CoreRegister, basic_block: &mut BasicBlock) {
        self.pointer.store_width4(src, basic_block);
    }

    pub fn get_value(&self, dst: CoreRegister, basic_block: &mut BasicBlock) {
        self.pointer.load_width4(dst, basic_block);
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
                self.pointer.load_width4(transfer_reg, basic_block);
                out.set_value_from_register(transfer_reg, basic_block);
                basic_block.free_register(transfer_reg);
                Box::new(out)
            }
        }
    }

    fn print(&self, basic_block: &mut BasicBlock) {
        self.pointer.load_width4(CoreRegister::R0, basic_block);
        basic_block.push(ArmIns::BranchAndLink { addr: "jprint_int" });
    }

    fn increment(&self, basic_block: &mut BasicBlock) {
        let increment_reg = basic_block.claim_register();
        self.pointer.load_width4(increment_reg, basic_block);
        basic_block.push(ArmIns::AddImm {
            dst: increment_reg,
            src: increment_reg,
            imm: 1
        });
        self.pointer.store_width4(increment_reg, basic_block);
        basic_block.free_register(increment_reg);
    }

    fn square(&self, basic_block: &mut BasicBlock) {
        let multiply_reg = basic_block.claim_register();
        self.pointer.load_width4(multiply_reg, basic_block);
        basic_block.push(ArmIns::Multiply {
            dst: multiply_reg,
            src: multiply_reg,
            mul: multiply_reg,
        });
        self.pointer.store_width4(multiply_reg, basic_block);
        basic_block.free_register(multiply_reg);
    }

    fn negate(&self, basic_block: &mut BasicBlock) {
        let temp_reg = basic_block.claim_register();
        let negation_reg = basic_block.claim_register();
        self.pointer.load_width4(temp_reg, basic_block);
        basic_block.push(ArmIns::MoveImm {
            dst: negation_reg, imm: 0 });
        // subtract the immediate from 0
        basic_block.push(ArmIns::Sub {
            dst: temp_reg,
            src: negation_reg,
            sub: temp_reg
        });
        self.pointer.store_width4(temp_reg, basic_block);
        basic_block.free_register(temp_reg);
        basic_block.free_register(negation_reg);
    }

    fn ceiling(&self, _basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        Box::new(self.clone())  // nothing to do; ceil(int) is always the given int
    }

    fn reciprocal(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> Box<dyn TypedValue> {
        // TODO: all of the registers used here need to be claimed from the BasicBlock
        let numerator = basic_block.stack_allocate_double();  // TODO: reclaim this as well
        let numerator_reg = ExtensionRegisterDoublePrecision::D0; // doesn't need to be this exact register, but must ensure no overlap with other extension registers
        numerator.set_value(1.0, basic_block, globalctx);
        numerator.get_value(numerator_reg, basic_block);
        let denom_reg_int = basic_block.claim_register();
        self.get_value(denom_reg_int, basic_block);
        let denom_reg_single_precision = ExtensionRegisterSinglePrecision::S15; // doesn't need to be this exact register, but must ensure no overlap with other extension registers
        basic_block.push(ArmIns::MoveCoreRegisterToSinglePrecisionExtensionRegister { src: denom_reg_int, dst: denom_reg_single_precision });
        let denom_reg_double_precision = ExtensionRegisterDoublePrecision::D1; // doesn't need to be this exact register, but must ensure no overlap with other extension registers
        basic_block.push(ArmIns::ConvertSinglePrecisionToDoublePrecision { src: denom_reg_single_precision, dst:  denom_reg_double_precision });
        let result_reg = ExtensionRegisterDoublePrecision::D2; // doesn't need to be this exact register, but must ensure no overlap with other extension registers
        basic_block.push(ArmIns::DivideDoublePrecision { dst: result_reg, numerator: numerator_reg, denominator: denom_reg_double_precision });
        let result = basic_block.stack_allocate_double();
        result.set_value_from_register(result_reg, basic_block);
        basic_block.free_register(denom_reg_int);
        Box::new(result)
    }

    fn sum(&self, addend: Box<dyn TypedValue>, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        match addend.type_flag() {
            TypeFlag::Double => unimplemented!("TODO: Support adding double to int."),
            TypeFlag::Integer => {
                let addend_int : &IntegerValue = addend.as_any().downcast_ref::<IntegerValue>().expect("an IntegerValue");
                let lhs_reg = basic_block.claim_register();
                let rhs_reg = basic_block.claim_register();
                self.pointer.load_width4(lhs_reg, basic_block);
                addend_int.pointer.load_width4(rhs_reg, basic_block);
                basic_block.push(ArmIns::Add { dst: rhs_reg, src: lhs_reg, add: rhs_reg });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.store_width4(rhs_reg, basic_block);
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
                self.pointer.load_width4(lhs_reg, basic_block);
                addend_int.pointer.load_width4(rhs_reg, basic_block);
                basic_block.push(ArmIns::Multiply { dst: rhs_reg, src: lhs_reg, mul: rhs_reg });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.store_width4(rhs_reg, basic_block);
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
                self.pointer.load_width4(lhs_reg, basic_block);
                addend_int.pointer.load_width4(rhs_reg, basic_block);
                basic_block.push(ArmIns::Sub { dst: rhs_reg, src: lhs_reg, sub: rhs_reg });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.store_width4(rhs_reg, basic_block);
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
                self.pointer.load_width4(lhs_reg, basic_block);
                addend_int.pointer.load_width4(rhs_reg, basic_block);
                basic_block.push(ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                basic_block.push(ArmIns::MoveLT { dst: rhs_reg, src: 1 });
                basic_block.push(ArmIns::MoveGE { dst: rhs_reg, src: 0 });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.store_width4(rhs_reg, basic_block);
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
                self.pointer.load_width4(lhs_reg, basic_block);
                addend_int.pointer.load_width4(rhs_reg, basic_block);
                basic_block.push(ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                basic_block.push(ArmIns::MoveGT { dst: rhs_reg, src: 1 });
                basic_block.push(ArmIns::MoveLE { dst: rhs_reg, src: 0 });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.store_width4(rhs_reg, basic_block);
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
                self.pointer.load_width4(lhs_reg, basic_block);
                addend_int.pointer.load_width4(rhs_reg, basic_block);
                basic_block.push(ArmIns::Compare { lhs: lhs_reg, rhs: rhs_reg });
                basic_block.push(ArmIns::MoveEQ { dst: rhs_reg, src: 1 });
                basic_block.push(ArmIns::MoveNE { dst: rhs_reg, src: 0 });
                let out_value = basic_block.stack_allocate_int();
                out_value.pointer.store_width4(rhs_reg, basic_block);
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
    pointer: Pointer,
}

impl DoubleValue {
    pub fn new(pointer: Pointer) -> DoubleValue {
        DoubleValue {
            ty: TypeFlag::Double,
            pointer
        }
    }

    pub fn set_value(&self, num: f64, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) {
        let (label, offset) = globalctx.get_double_constant_pool_offset(num);
        let transfer_reg = ExtensionRegisterDoublePrecision::D14;
        basic_block.push(ArmIns::LoadDoublePrecisionRegisterFromLabel {
            src: label.to_string(),
            dst: transfer_reg,
            offset
        });
        self.pointer.store_width8(transfer_reg, basic_block);
    }

    pub fn get_value(&self, dst: ExtensionRegisterDoublePrecision, basic_block: &mut BasicBlock) {
        self.pointer.load_width8(dst, basic_block);
    }

    pub fn set_value_from_register(&self, src: ExtensionRegisterDoublePrecision, basic_block: &mut BasicBlock) {
        self.pointer.store_width8(src, basic_block);
    }
}

impl TypedValue for DoubleValue {

    fn type_flag(&self) -> TypeFlag {
        self.ty
    }

    fn is_entirely_on_heap(&self) -> bool {
        match &self.pointer {
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
        match &self.pointer {
            Pointer::Heap(_) => Box::new(self.clone()), // TODO: increment refcount
            Pointer::Stack(_) => {
                let out = globalctx.heap_allocate_double();
                let transfer_reg = ExtensionRegisterDoublePrecision::D12;
                self.pointer.load_width8(transfer_reg, basic_block);
                out.pointer.store_width8(transfer_reg, basic_block);
                Box::new(out)
            }
        }
    }

    fn print(&self, basic_block: &mut BasicBlock) {
        self.get_value(ExtensionRegisterDoublePrecision::D0, basic_block);
        basic_block.push(ArmIns::BranchAndLink { addr: "jprint_double" });
    }

    fn increment(&self, _basic_block: &mut BasicBlock) {
        unimplemented!("TODO: implement DoubleValue::increment")
    }

    fn square(&self, _basic_block: &mut BasicBlock) {
        unimplemented!("TODO: implement DoubleValue::square")
    }

    fn negate(&self, basic_block: &mut BasicBlock) {
        let transfer_reg = ExtensionRegisterDoublePrecision::D2; // TODO: claim this from the BasicBlock
        self.get_value(transfer_reg, basic_block);
        basic_block.push(ArmIns::NegateDoublePrecision { dst: transfer_reg, operand: transfer_reg });
        self.set_value_from_register(transfer_reg, basic_block);
    }

    fn ceiling(&self, basic_block: &mut BasicBlock) -> Box<dyn TypedValue> {
        self.get_value(ExtensionRegisterDoublePrecision::D0, basic_block);
        basic_block.push(ArmIns::BranchAndLink { addr: "jceiling" });
        let new_value = basic_block.stack_allocate_int();
        new_value.set_value_from_register(CoreRegister::R0, basic_block);
        // TODO: we should decrement the old value's refcount so we can reuse its space later on.
        Box::new(new_value)
    }

    fn reciprocal(&self, basic_block: &mut BasicBlock, globalctx: &mut GlobalContext) -> Box<dyn TypedValue> {
        todo!()
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
        f.write_fmt(format_args!("DoubleValue<pointer={}>", self.pointer))
    }
}
