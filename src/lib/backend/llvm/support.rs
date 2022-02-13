use llvm_sys::core::*;
use llvm_sys::prelude::*;

use std::os::raw::{c_double, c_ulonglong};

pub const LLVM_FALSE: LLVMBool = 0;

pub unsafe fn int1(val: c_ulonglong) -> LLVMValueRef {
    LLVMConstInt(LLVMInt1Type(), val, LLVM_FALSE)
}

/// Convert this integer to LLVM's representation of a constant
/// integer.
pub unsafe fn int8(val: c_ulonglong) -> LLVMValueRef {
    LLVMConstInt(LLVMInt8Type(), val, LLVM_FALSE)
}

pub unsafe fn f64(val: c_double) -> LLVMValueRef {
    LLVMConstReal(LLVMDoubleType(), val)
}

/// Convert this integer to LLVM's representation of a constant
/// integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
pub fn int32(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt32Type(), val, LLVM_FALSE) }
}

pub fn int64(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt64Type(), val, LLVM_FALSE) }
}

pub fn int1_type() -> LLVMTypeRef {
    unsafe { LLVMInt1Type() }
}

pub fn int8_type() -> LLVMTypeRef {
    unsafe { LLVMInt8Type() }
}

pub fn int32_type() -> LLVMTypeRef {
    unsafe { LLVMInt32Type() }
}

pub fn f64_type() -> LLVMTypeRef {
    unsafe { LLVMDoubleType() }
}

pub fn f64_ptr_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMPointerType(LLVMDoubleType(), 0), 0) }
}

pub fn int8_ptr_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMPointerType(LLVMInt8Type(), 0), 0) }
}

pub fn int32_ptr_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMPointerType(LLVMInt32Type(), 0), 0) }
}

pub fn void_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMVoidType(), 0) }
}
