use itertools::Itertools;
use std::collections::HashMap;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::transforms::pass_manager_builder::*;
use llvm_sys::{LLVMBuilder, LLVMModule};

use std::ffi::{CStr, CString};
use std::os::raw::{c_uint, c_ulonglong, c_double};
use std::ptr::null_mut;
use std::str;

use parser;

const LLVM_FALSE: LLVMBool = 0;

pub fn compile_to_module(
    module_name: &str,
    target_triple: Option<String>,
    ast: &[parser::AstNode],
) -> Module {
    let mut module = create_module(module_name, target_triple);

    let main_bb = add_main_fn(&mut module);

    unsafe {
        for astnode in ast {

            // Each top-level statement gets its own function,
            // as each statement gets its own stack frame.
            let (stmt_fn, stmt_bb) = add_stmt_fn(&mut module);

            let jval = match astnode {
                parser::AstNode::Print(expr) => {
                    let c_expr = compile_expr(expr, &mut module, stmt_bb);
                    match expr.as_ref() {
                        // Top level global assignments aren't printed to console...
                        parser::AstNode::IsGlobal {ident: _, expr: _} => (),
                        // ... all other statements are:
                        _ => {
                            let mut args = vec![c_expr.ptr, int1(1)];
                            add_function_call(&mut module, stmt_bb, "jprint", &mut args[..], "");
                        }
                    }
                    c_expr
                }
                _ => panic!("Not ready to compile top-level AST node: {:?}", astnode),
            };

            // After each top-level statement, drop the resulting JVal to free up memory.
            // Notice that 0/false is passed as second param: we don't drop globals until end of
            // program/global scope.
            let mut args = vec![jval.ptr, int1(0)];
            add_function_call(&mut module, stmt_bb, "jval_drop", &mut args[..], "");

            // Return from the top-level stmt function.
            let stmt_builder = Builder::new();
            stmt_builder.position_at_end(stmt_bb);
            LLVMBuildRetVoid(stmt_builder.builder);

            // Call the statement function from main.
            let main_builder = Builder::new();
            main_builder.position_at_end(main_bb);
            LLVMBuildCall(
                main_builder.builder,
                stmt_fn,
                Vec::new().as_mut_ptr(),
                0,
                module.new_string_ptr(""),
            );
        }

        // End of global scope: drop all globals.
        let mut args = vec![];
        add_function_call(&mut module, main_bb, "jglobals_dropall", &mut args[..], "");

        // Enforce memory cleanliness requirements.
        let mut args = vec![];
        add_function_call(&mut module, main_bb, "jmemory_enforce", &mut args[..], "");

        add_main_cleanup(main_bb);

        module
    }
}

#[derive(Clone)]
enum JValType {
    Integer = 1,
    Array = 2,
    DoublePrecisionFloat = 3,
}

#[derive(Clone)]
enum JValLocation {
    Stack = 1,
//    HeapLocal = 2,
    HeapGlobal = 3,
}

#[derive(Clone)]
struct JValPtr {
    // TODO: Think of compile-time optimizations for which the
    // static info recorded in this struct could be useful.
    static_type : Option<JValType>,   // the type (if known at compile time)
    static_len: Option<u64>,          // the length (if known at compile time)
    ptr: LLVMValueRef,                // pointer to a JVal struct
}

fn alloc_jval(
    module: &mut Module,
    bb: LLVMBasicBlockRef,
    val: LLVMValueRef,
    val_type: JValType,
    val_loc: JValLocation,
    val_len: u64) -> JValPtr {

    let builder = Builder::new();
    builder.position_at_end(bb);

    unsafe {
        // Build a JVal struct.
        let jval_ptr = LLVMBuildAlloca(
            builder.builder,
            module.jval_struct_type,
            module.new_string_ptr("jval")
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

        // Indicate the location.
        let mut loc_offset = vec![int64(0), int32(1)];
        let loc_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            loc_offset.as_mut_ptr(),
            loc_offset.len() as u32,
            module.new_string_ptr("jval_loc_gep"),
        );
        LLVMBuildStore(builder.builder, int8(val_loc.clone() as u64), loc_gep);

        // Indicate the length.
        let mut len_offset = vec![int64(0), int32(2)];
        let len_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            len_offset.as_mut_ptr(),
            len_offset.len() as u32,
            module.new_string_ptr("jval_len_gep"),
        );
        LLVMBuildStore(builder.builder, int32(val_len), len_gep);

        // Point to the value.
        let mut ptr_offset = vec![int64(0), int32(3)];
        let ptr_gep = LLVMBuildInBoundsGEP(
            builder.builder,
            jval_ptr,
            ptr_offset.as_mut_ptr(),
            ptr_offset.len() as u32,
            module.new_string_ptr("jval_ptr_gep"),
        );
        let ptr_gep = match val_type {
            JValType::Integer =>
                LLVMBuildPointerCast(
                    builder.builder,
                    ptr_gep,
                    int32_ptr_ptr_type(),
                    module.new_string_ptr("jval_ptr_cast")
                )
            ,
            JValType::DoublePrecisionFloat =>
                LLVMBuildPointerCast(
                    builder.builder,
                    ptr_gep,
                    f64_ptr_ptr_type(),
                    module.new_string_ptr("jval_ptr_cast")
                )
            , _ => ptr_gep
        };
        LLVMBuildStore(builder.builder, val, ptr_gep);

        JValPtr { static_type : Some(val_type), static_len : Some(val_len), ptr : jval_ptr }
    }
}

fn compile_expr(
    expr: &parser::AstNode,
    module: &mut Module,
    bb: LLVMBasicBlockRef,
) -> JValPtr {
    let builder = Builder::new();
    builder.position_at_end(bb);
    match *expr {
        parser::AstNode::Integer(n) => {
            unsafe {
                // Allocate space for the number.
                let num = LLVMBuildAlloca(
                    builder.builder,
                    int32_type(),
                    module.new_string_ptr("int_alloc")
                );
                LLVMBuildStore(builder.builder, int32(n as u64), num);

                // Point to the number via a JVal struct.
                let ty = JValType::Integer;
                alloc_jval(module, bb, num, ty.clone(), JValLocation::Stack, 1)
            }
        },
        parser::AstNode::DoublePrecisionFloat(n) => {
            unsafe {
                // Allocate space for the number.
                let num = LLVMBuildAlloca(
                    builder.builder,
                    f64_type(),
                    module.new_string_ptr("dblfp_alloc")
                );
                LLVMBuildStore(builder.builder, f64(n), num);

                // Point to the number via a JVal struct.
                let ty = JValType::DoublePrecisionFloat;
                alloc_jval(module, bb, num, ty.clone(), JValLocation::Stack, 1)
            }
        },
        parser::AstNode::Terms(ref terms) => {

            // Ensure we have two or more terms to assemble into an array.
            // Zero terms should be syntactically impossible;
            // single terms should be unwrapped by the parser.
            assert!(terms.len() >= 2);

            // Compile the terms.
            let compiled_terms = terms
                .iter()
                .map(|t| compile_expr(t, module, bb))
                .collect_vec();

            unsafe {
                // Allocate an array to hold the terms.
                let arr = LLVMBuildArrayAlloca(
                    builder.builder,
                    module.jval_ptr_type,
                    int64(compiled_terms.len() as u64),
                    module.new_string_ptr("terms_arr")
                );

                // Load pointers to each JVal into the array.
                for (idx, jval) in compiled_terms.iter().enumerate() {
                      let mut args = vec![arr, jval.ptr, int32(idx as u64)];
                      add_function_call(module, bb, "jexpand", &mut args[..], "");
                }

                // Point to the array via a JVal struct.
                let ty = JValType::Array;
                alloc_jval(module, bb, arr, ty.clone(), JValLocation::Stack, compiled_terms.len() as u64)
            }
        },
        parser::AstNode::MonadicOp {ref verb, ref expr} => {
            let expr = compile_expr(expr, module, bb);
            unsafe {
                let mut args = vec![int8(verb.clone() as u64), expr.ptr];
                let monad_op_arr = add_function_call(module, bb, "jmonad", &mut args[..], "monad_op_arr");

                // Drop the operand.
                let mut args = vec![expr.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");

                JValPtr { static_type : None, static_len : None, ptr : monad_op_arr }
            }
        },
        parser::AstNode::DyadicOp {ref verb, ref lhs, ref rhs} => {

            let mut rhs = compile_expr(rhs, module, bb);
            let mut lhs = compile_expr(lhs, module, bb);

            // Pass args to dynamic library function; types/lengths will be resolved there.
            // TODO: If both type and len of lhs and rhs are statically known,
            // optimize by performing additions without function call overhead.
            unsafe {
                let mut args = vec![int8(verb.clone() as u64), lhs.ptr, rhs.ptr];
                let dyad_op_arr = add_function_call(
                    module, bb, "jdyad", &mut args[..], "dyad_op_arr");

                // Drop the operands.
                let mut args = vec![lhs.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");
                let mut args = vec![rhs.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");

                JValPtr { static_type : None, static_len : None, ptr : dyad_op_arr }
            }
        },
        parser::AstNode::Reduce{ ref verb, ref expr } => {

            let mut expr = compile_expr(expr, module, bb);

            // Pass args to dynamic library function; types/lengths will be resolved there.
            // TODO: If both type and len of lhs and rhs are statically known,
            // optimize by performing additions without function call overhead.
            unsafe {
                let mut args = vec![int8(verb.clone() as u64), expr.ptr];
                let reduced_arr = add_function_call(
                    module, bb, "jreduce", &mut args[..], "reduced_arr");

                // Drop the operand.
                let mut args = vec![expr.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");

                JValPtr { static_type : None, static_len : None, ptr : reduced_arr }
            }
        },
        parser::AstNode::IsGlobal{ ref ident, ref expr } => {
            let mut expr = compile_expr(expr, module, bb);

            // IMPORTANT: For an assignment sequence such as:
            //   z =: 6
            //   z =: 8
            // this code will clone 8 into a global, then free the non-global 8,
            // and finally free 6. The issue is that
            // 6 could be referred to elsewhere, thus we eventually need reference counting.

            // Clone the JVal into the global heap space.
            let mut args = vec![expr.ptr, int64(JValLocation::HeapGlobal as u64)];
            let global_clone = unsafe { add_function_call(module, bb, "jval_clone", &mut args[..], "") };

            // Drop the underlying expression
            unsafe {
                let mut args = vec![expr.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");
            }

            // Register identifier; if already registered, will get existing id assigned,
            // otherwise unique one will be assigned to it.
            let global_id = module.get_or_assign_id_to_global_ident(&ident[..]);

            // Set the global identifier to refer to the cloned value.
            // Note that this will drop any prior referred to values behind the scenes
            // and will cause future problems; see note there.
            unsafe {
                let mut args = vec![int32(global_id as u64), global_clone];
                add_function_call(module, bb, "jglobal_set_reference", &mut args[..], "");
            }

            let global = JValPtr { static_type : None, static_len : None, ptr : global_clone };
            global
        },
        parser::AstNode::Ident(ref ident) => {
            let global_id = module.get_or_assign_id_to_global_ident(ident);
            unsafe {
                let mut args = vec![int32(global_id as u64)];
                let global_ref = add_function_call(module, bb, "jglobal_get_reference", &mut args[..], "global_ref");
                JValPtr { static_type : None, static_len : None, ptr : global_ref}
            }
        },
        _ => unimplemented!("Not ready to compile expr: {:?}", expr),
    }
}

/// A struct that keeps ownership of all the strings we've passed to
/// the LLVM API until we destroy the `LLVMModule`.
pub struct Module {
    module: *mut LLVMModule,
    strings: Vec<CString>,
    global_scope_idents: HashMap<String, u32>,
    jval_struct_type: LLVMTypeRef,
    jval_ptr_type: LLVMTypeRef,
}

impl Module {
    /// Create a new CString associated with this LLVMModule,
    /// and return a pointer that can be passed to LLVM APIs.
    /// Assumes s is pure-ASCII.
    fn new_string_ptr(&mut self, s: &str) -> *const i8 {
        self.new_mut_string_ptr(s)
    }

    // TODO: ideally our pointers wouldn't be mutable.
    fn new_mut_string_ptr(&mut self, s: &str) -> *mut i8 {
        let cstring = CString::new(s).unwrap();
        let ptr = cstring.as_ptr() as *mut _;
        self.strings.push(cstring);
        ptr
    }

    pub fn to_cstring(&self) -> CString {
        unsafe {
            // LLVM gives us a *char pointer, so wrap it in a CStr to mark it
            // as borrowed.
            let llvm_ir_ptr = LLVMPrintModuleToString(self.module);
            let llvm_ir = CStr::from_ptr(llvm_ir_ptr as *const _);

            // Make an owned copy of the string in our memory space.
            let module_string = CString::new(llvm_ir.to_bytes()).unwrap();

            // Cleanup borrowed string.
            LLVMDisposeMessage(llvm_ir_ptr);

            module_string
        }
    }

    fn get_or_assign_id_to_global_ident(&mut self, ident: &str) -> u32 {
        let current_len = self.global_scope_idents.len() as u32;
        *self.global_scope_idents.entry(String::from(ident))
            .or_insert(current_len)
    }

}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.module);
        }
    }
}

/// Wraps LLVM's builder class to provide a nicer API and ensure we
/// always dispose correctly.
struct Builder {
    builder: *mut LLVMBuilder,
}

impl Builder {
    /// Create a new Builder in LLVM's global context.
    fn new() -> Self {
        unsafe {
            Builder {
                builder: LLVMCreateBuilder(),
            }
        }
    }

    fn position_at_end(&self, bb: LLVMBasicBlockRef) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

#[derive(Clone)]
struct CompileContext {
    cells: LLVMValueRef,
    cell_index_ptr: LLVMValueRef,
    main_fn: LLVMValueRef,
}

unsafe fn int1(val: c_ulonglong) -> LLVMValueRef {
    LLVMConstInt(LLVMInt1Type(), val, LLVM_FALSE)
}

/// Convert this integer to LLVM's representation of a constant
/// integer.
unsafe fn int8(val: c_ulonglong) -> LLVMValueRef {
    LLVMConstInt(LLVMInt8Type(), val, LLVM_FALSE)
}

unsafe fn f64(val: c_double) -> LLVMValueRef {
    LLVMConstReal(LLVMDoubleType(), val)
}

/// Convert this integer to LLVM's representation of a constant
/// integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
fn int32(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt32Type(), val, LLVM_FALSE) }
}

fn int64(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt64Type(), val, LLVM_FALSE) }
}

fn int1_type() -> LLVMTypeRef {
    unsafe { LLVMInt1Type() }
}

fn int8_type() -> LLVMTypeRef {
    unsafe { LLVMInt8Type() }
}

fn int32_type() -> LLVMTypeRef {
    unsafe { LLVMInt32Type() }
}

fn f64_type() -> LLVMTypeRef {
    unsafe { LLVMDoubleType() }
}

fn f64_ptr_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMPointerType(LLVMDoubleType(), 0), 0) }
}

fn int32_ptr_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMPointerType(LLVMInt32Type(), 0), 0) }
}

fn void_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMVoidType(), 0) }
}

fn add_function(
    module: &mut Module,
    fn_name: &str,
    args: &mut [LLVMTypeRef],
    ret_type: LLVMTypeRef,
) {
    unsafe {
        let fn_type = LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, LLVM_FALSE);
        LLVMAddFunction(module.module, module.new_string_ptr(fn_name), fn_type);
    }
}


fn add_c_declarations(module: &mut Module) {
    let void;
    unsafe {
        void = LLVMVoidType();
    }

//    add_function(
//        module,
//        "llvm.memset.p0i8.i32",
//        &mut [
//            int8_ptr_type(),
//            int8_type(),
//            int32_type(),
//            int32_type(),
//            int1_type(),
//        ],
//        void,
//    );

//    add_function(module, "malloc", &mut [int32_type()], int8_ptr_type());
//
//    add_function(module, "free", &mut [int8_ptr_type()], void);
//
//    add_function(
//        module,
//        "write",
//        &mut [int32_type(), int8_ptr_type(), int32_type()],
//        int32_type(),
//    );

//    add_function(module, "putchar", &mut [int32_type()], int32_type());

//    add_function(module, "getchar", &mut [], int32_type());

//    add_function(
//        module,
//        "printf",
//        &mut [
//            int8_ptr_type(), /* varargs; simply give extra args at call time. */
//        ],
//        int32_type(),
//    );

    let jval_ptr_type = module.jval_ptr_type.clone();
    let jval_ptr_ptr_type = unsafe { LLVMPointerType(
        module.jval_ptr_type.clone(),
        0
    )};

    add_function(module, "jprint", & mut [jval_ptr_type, int1_type()], void);
    add_function(module, "jexpand", &mut [jval_ptr_ptr_type, jval_ptr_type, int32_type()], void);
    add_function(module, "jmonad", & mut [int8_type(), jval_ptr_type], jval_ptr_type);
    add_function(module, "jdyad", & mut [int8_type(), jval_ptr_type, jval_ptr_type], jval_ptr_type);
    add_function(module, "jreduce", & mut [int8_type(), jval_ptr_type], jval_ptr_type);
    add_function(module, "jval_drop", & mut [jval_ptr_type, int1_type()], void);
    add_function(module, "jval_clone", & mut [jval_ptr_type, int8_type()], jval_ptr_type);
    add_function(module, "jmemory_enforce", & mut [], void);

    add_function(module, "jglobal_set_reference", & mut [int32_type(), jval_ptr_ptr_type], void);
    add_function(module, "jglobal_get_reference", & mut [int32_type()], jval_ptr_type);
    add_function(module, "jglobals_dropall", & mut [], void);
}

unsafe fn add_function_call(
    module: &mut Module,
    bb: LLVMBasicBlockRef,
    fn_name: &str,
    args: &mut [LLVMValueRef],
    name: &str,
) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);

    let function = LLVMGetNamedFunction(module.module, module.new_string_ptr(fn_name));

    LLVMBuildCall(
        builder.builder,
        function,
        args.as_mut_ptr(),
        args.len() as c_uint,
        module.new_string_ptr(name),
    )
}

fn create_module(module_name: &str, target_triple: Option<String>) -> Module {

    let mut strings = Vec::new();

    let c_module_name = CString::new(module_name).unwrap();
    let module_name_char_ptr = c_module_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_module_name);

    let c_jval_struct_name = CString::new("jval_struct").unwrap();
    let jval_struct_name_char_ptr = c_jval_struct_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_jval_struct_name);

    let c_alive_heap_jval_counter_name = CString::new("alive_heap_jval_counter").unwrap();
    let alive_heap_jval_counter_name = c_alive_heap_jval_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_alive_heap_jval_counter_name);

    let c_alive_heap_int_counter_name = CString::new("alive_heap_int_counter").unwrap();
    let alive_heap_int_counter_name = c_alive_heap_int_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_alive_heap_int_counter_name);

    let c_alive_heap_double_counter_name = CString::new("alive_heap_double_counter").unwrap();
    let alive_heap_double_counter_name = c_alive_heap_double_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_alive_heap_double_counter_name);

    let c_alive_heap_jvalptrarray_counter_name = CString::new("alive_heap_jvalptrarray_counter").unwrap();
    let alive_heap_jvalptrarray_counter_name = c_alive_heap_jvalptrarray_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_alive_heap_jvalptrarray_counter_name);

    let c_total_heap_jval_counter_name = CString::new("total_heap_jval_counter").unwrap();
    let total_heap_jval_counter_name = c_total_heap_jval_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_total_heap_jval_counter_name);

    let c_total_heap_int_counter_name = CString::new("total_heap_int_counter").unwrap();
    let total_heap_int_counter_name = c_total_heap_int_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_total_heap_int_counter_name);

    let c_total_heap_double_counter_name = CString::new("total_heap_double_counter").unwrap();
    let total_heap_double_counter_name = c_total_heap_double_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_total_heap_double_counter_name);

    let c_total_heap_jvalptrarray_counter_name = CString::new("total_heap_jvalptrarray_counter").unwrap();
    let total_heap_jvalptrarray_counter_name = c_total_heap_jvalptrarray_counter_name.to_bytes_with_nul().as_ptr() as *const _;
    strings.push(c_total_heap_jvalptrarray_counter_name);

    let mut module = unsafe {
        let llvm_module = LLVMModuleCreateWithName(module_name_char_ptr);

        let global_ctx = LLVMGetGlobalContext();
        let jval_struct_type = LLVMStructCreateNamed(
            global_ctx,
            jval_struct_name_char_ptr,
        );
        // IMPORTANT: Be sure this matches up with the corresponding definition
        // in jverbs.c.
        let mut members = vec![
            int8_type(),        // the value's type
            int8_type(),        // the value's location
            int32_type(),       // the value's length
            void_ptr_type()    // a pointer to the value
        ];
        LLVMStructSetBody(
            jval_struct_type,
            members.as_mut_ptr(),
            members.len() as u32,
            0
        );

        let alive_heap_jval_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            alive_heap_jval_counter_name,
        );
        LLVMSetInitializer(alive_heap_jval_counter, int32(0));

        let alive_heap_int_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            alive_heap_int_counter_name,
        );
        LLVMSetInitializer(alive_heap_int_counter, int32(0));

        let alive_heap_double_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            alive_heap_double_counter_name,
        );
        LLVMSetInitializer(alive_heap_double_counter, int32(0));

        let alive_heap_jvalptrarray_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            alive_heap_jvalptrarray_counter_name,
        );
        LLVMSetInitializer(alive_heap_jvalptrarray_counter, int32(0));

        let total_heap_jval_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            total_heap_jval_counter_name,
        );
        LLVMSetInitializer(total_heap_jval_counter, int32(0));

        let total_heap_int_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            total_heap_int_counter_name,
        );
        LLVMSetInitializer(total_heap_int_counter, int32(0));

        let total_heap_double_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            total_heap_double_counter_name,
        );
        LLVMSetInitializer(total_heap_double_counter, int32(0));

        let total_heap_jvalptrarray_counter = LLVMAddGlobal(
            llvm_module,
            int32_type(),
            total_heap_jvalptrarray_counter_name,
        );
        LLVMSetInitializer(total_heap_jvalptrarray_counter, int32(0));

        Module {
            module: llvm_module,
            strings,
            global_scope_idents: HashMap::new(),
            jval_struct_type,
            jval_ptr_type : LLVMPointerType(jval_struct_type, 0),
        }
    };

    let target_triple_cstring = if let Some(target_triple) = target_triple {
        CString::new(target_triple).unwrap()
    } else {
        get_default_target_triple()
    };

    // This is necessary for maximum LLVM performance, see
    // http://llvm.org/docs/Frontend/PerformanceTips.html
    unsafe {
        LLVMSetTarget(module.module, target_triple_cstring.as_ptr() as *const _);
    }
    // TODO: add a function to the LLVM C API that gives us the
    // data layout from the target machine.

    add_c_declarations(&mut module);
    module
}

fn add_main_fn(module: &mut Module) -> LLVMBasicBlockRef {
    let mut main_args = vec![];
    unsafe {
        let main_type = LLVMFunctionType(int32_type(), main_args.as_mut_ptr(), 0, LLVM_FALSE);
        // TODO: use add_function() here instead.
        let main_fn = LLVMAddFunction(module.module, module.new_string_ptr("main"), main_type);
        LLVMAppendBasicBlock(main_fn, module.new_string_ptr(""))
    }
}

fn add_stmt_fn(module: &mut Module) -> (LLVMValueRef, LLVMBasicBlockRef) {
    let mut args = vec![];
    unsafe {
        let stmt_type = LLVMFunctionType(LLVMVoidType(), args.as_mut_ptr(), 0, LLVM_FALSE);
        // TODO: use add_function() here instead.
        let stmt_fn = LLVMAddFunction(module.module, module.new_string_ptr("stmt"), stmt_type);
        let stmt_bb = LLVMAppendBasicBlock(stmt_fn, module.new_string_ptr(""));
        (stmt_fn, stmt_bb)
    }
}

/// Add prologue to main function.
unsafe fn add_main_cleanup(bb: LLVMBasicBlockRef) {
    let builder = Builder::new();
    builder.position_at_end(bb);

    let zero = int32(0);
    LLVMBuildRet(builder.builder, zero);
}

pub fn optimise_ir(module: &mut Module, llvm_opt: i64) {
    // TODO: add a verifier pass too.
    unsafe {
        let builder = LLVMPassManagerBuilderCreate();
        // E.g. if llvm_opt is 3, we want a pass equivalent to -O3.
        LLVMPassManagerBuilderSetOptLevel(builder, llvm_opt as u32);

        let pass_manager = LLVMCreatePassManager();
        LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_manager);

        LLVMPassManagerBuilderDispose(builder);

        // Run twice. This is a hack, we should really work out which
        // optimisations need to run twice. See
        // http://llvm.org/docs/Frontend/PerformanceTips.html#pass-ordering
        LLVMRunPassManager(pass_manager, module.module);
        LLVMRunPassManager(pass_manager, module.module);

        LLVMDisposePassManager(pass_manager);
    }
}

pub fn get_default_target_triple() -> CString {
    let target_triple;
    unsafe {
        let target_triple_ptr = LLVMGetDefaultTargetTriple();
        target_triple = CStr::from_ptr(target_triple_ptr as *const _).to_owned();
        LLVMDisposeMessage(target_triple_ptr);
    }

    target_triple
}

struct TargetMachine {
    tm: LLVMTargetMachineRef,
}

impl TargetMachine {
    fn new(target_triple: *const i8) -> Result<Self, String> {
        let mut target = null_mut();
        let mut err_msg_ptr = null_mut();
        unsafe {
            LLVMGetTargetFromTriple(target_triple, &mut target, &mut err_msg_ptr);
            if target.is_null() {
                // LLVM couldn't find a target triple with this name,
                // so it should have given us an error message.
                assert!(!err_msg_ptr.is_null());

                let err_msg_cstr = CStr::from_ptr(err_msg_ptr as *const _);
                let err_msg = str::from_utf8(err_msg_cstr.to_bytes()).unwrap();
                return Err(err_msg.to_owned());
            }
        }

        // TODO: do these strings live long enough?
        // cpu is documented: http://llvm.org/docs/CommandGuide/llc.html#cmdoption-mcpu
        let cpu = CString::new("generic").unwrap();
        // features are documented: http://llvm.org/docs/CommandGuide/llc.html#cmdoption-mattr
        let features = CString::new("").unwrap();

        let target_machine;
        unsafe {
            target_machine = LLVMCreateTargetMachine(
                target,
                target_triple,
                cpu.as_ptr() as *const _,
                features.as_ptr() as *const _,
                LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
                LLVMRelocMode::LLVMRelocPIC,
                LLVMCodeModel::LLVMCodeModelDefault,
            );
        }

        Ok(TargetMachine { tm: target_machine })
    }
}

impl Drop for TargetMachine {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetMachine(self.tm);
        }
    }
}

pub fn write_object_file(module: &mut Module, path: &str) -> Result<(), String> {
    unsafe {
        let target_triple = LLVMGetTarget(module.module);

        // TODO: are all these necessary? Are there docs?
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();

        let target_machine = TargetMachine::new(target_triple).unwrap();

        let mut obj_error = module.new_mut_string_ptr("Writing object file failed.");
        let result = LLVMTargetMachineEmitToFile(
            target_machine.tm,
            module.module,
            module.new_string_ptr(path) as *mut i8,
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut obj_error,
        );

        if result != 0 {
            panic!("obj_error: {:?}", CStr::from_ptr(obj_error as *const _));
        }
    }
    Ok(())
}
