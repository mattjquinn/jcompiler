use llvm_sys::core::*;
use llvm_sys::prelude::*;

use super::compiler::{Builder, Module};
use super::support::{int32_type, int32, int8, int64, int32_ptr_ptr_type, f64_ptr_ptr_type, int8_ptr_ptr_type};

#[derive(Clone)]
pub enum JValType {
    Integer = 1,
    DoublePrecisionFloat = 2,
    Character = 3,
    NDimensionalArray = 4,
}

#[derive(Clone)]
pub enum JValTypeParam {
    Numeric = 1,
    String = 2,
}

#[derive(Clone)]
pub enum JValLocation {
    Stack = 1,
    //    HeapLocal = 2,
    HeapGlobal = 3,
}

#[derive(Clone)]
pub struct JValPtr {
    // TODO: Think of compile-time optimizations for which the
    // static info recorded in this struct could be useful.
    //static_type : Option<JValType>,   // the type (if known at compile time)
    //static_len: Option<u64>,          // the length (if known at compile time)
    pub ptr: LLVMValueRef, // pointer to a JVal struct
}

pub fn alloc_jval(
    module: &mut Module,
    bb: LLVMBasicBlockRef,
    val: LLVMValueRef,
    val_type: JValType,
    val_type_param: JValTypeParam,
    val_loc: JValLocation,
    val_shape: Vec<u64>,
) -> JValPtr {
    let builder = Builder::new();
    builder.position_at_end(bb);

    unsafe {
        // Build a JVal struct.
        let jval_ptr = LLVMBuildAlloca(
            builder.builder,
            module.jval_struct_type,
            module.new_string_ptr("jval"),
        );

        // Indicate the type.
        let mut type_offset = vec![int64(0), int32(0)];
        let type_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            type_offset.as_mut_ptr(),
            type_offset.len() as u32,
            module.new_string_ptr("jval_type_gep"),
        );
        LLVMBuildStore(builder.builder, int8(val_type.clone() as u64), type_gep);

        // Specify type parameter.
        let mut typaram_offset = vec![int64(0), int32(1)];
        let type_param_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            typaram_offset.as_mut_ptr(),
            typaram_offset.len() as u32,
            module.new_string_ptr("jval_typeparam_gep"),
        );
        LLVMBuildStore(builder.builder, int8(val_type_param as u64), type_param_gep);

        // Indicate the location.
        let mut loc_offset = vec![int64(0), int32(2)];
        let loc_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            loc_offset.as_mut_ptr(),
            loc_offset.len() as u32,
            module.new_string_ptr("jval_loc_gep"),
        );
        LLVMBuildStore(builder.builder, int8(val_loc.clone() as u64), loc_gep);

        // Indicate the rank.
        let mut rank_offset = vec![int64(0), int32(3)];
        let rank_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            rank_offset.as_mut_ptr(),
            rank_offset.len() as u32,
            module.new_string_ptr("jval_rank_gep"),
        );
        LLVMBuildStore(builder.builder, int32(val_shape.len() as u64), rank_gep);

        // Indicate the shape. First we need an array of integers...
        let shape_arr = LLVMBuildArrayAlloca(
            builder.builder,
            int32_type(),
            int64(val_shape.len() as u64),
            module.new_string_ptr("shape_arr"),
        );
        // ...now fill that array with provided shape dimensions...
        for (idx, dim) in val_shape.iter().cloned().enumerate() {
            let mut indices = vec![int32(idx as u64)];
            let dim_gep = LLVMBuildInBoundsGEP(
                builder.builder,
                shape_arr,
                indices.as_mut_ptr(),
                indices.len() as u32,
                module.new_string_ptr("dim_gep"),
            );
            LLVMBuildStore(builder.builder, int32(dim), dim_gep);
        }
        //...and finally, point to the shape array from the struct:
        let mut shape_offset = vec![int64(0), int32(4)];
        let shape_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            shape_offset.as_mut_ptr(),
            shape_offset.len() as u32,
            module.new_string_ptr("jval_shape_gep"),
        );
        LLVMBuildStore(builder.builder, shape_arr, shape_gep);

        // Point to the value.
        let mut ptr_offset = vec![int64(0), int32(5)];
        let ptr_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            ptr_offset.as_mut_ptr(),
            ptr_offset.len() as u32,
            module.new_string_ptr("jval_ptr_gep"),
        );
        let ptr_gep = match val_type {
            JValType::Integer => LLVMBuildPointerCast(
                builder.builder,
                ptr_gep,
                int32_ptr_ptr_type(),
                module.new_string_ptr("jval_ptr_cast"),
            ),
            JValType::DoublePrecisionFloat => LLVMBuildPointerCast(
                builder.builder,
                ptr_gep,
                f64_ptr_ptr_type(),
                module.new_string_ptr("jval_ptr_cast"),
            ),
            JValType::Character => LLVMBuildPointerCast(
                builder.builder,
                ptr_gep,
                int8_ptr_ptr_type(),
                module.new_string_ptr("jval_ptr_cast"),
            ),
            _ => ptr_gep,
        };
        LLVMBuildStore(builder.builder, val, ptr_gep);

        JValPtr { ptr: jval_ptr }
    }
}

