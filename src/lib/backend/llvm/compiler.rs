use itertools::Itertools;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::transforms::pass_manager_builder::*;
use llvm_sys::{LLVMBuilder, LLVMModule};
use std::collections::HashMap;

use std::ffi::{CStr, CString};
use std::os::raw::c_uint;
use std::str;

use parser;

use super::get_default_target_triple;
use super::runtime::{alloc_jval, JValLocation, JValPtr, JValType, JValTypeParam};
use super::support::{
    f64, f64_type, int1, int1_type, int32, int32_type, int64, int8, int8_type, void_ptr_type,
    LLVM_FALSE,
};

/// A struct that keeps ownership of all the strings we've passed to
/// the LLVM API until we destroy the `LLVMModule`.
pub struct Module {
    pub module: *mut LLVMModule,
    strings: Vec<CString>,
    global_scope_idents: HashMap<String, u32>,
    pub jval_struct_type: LLVMTypeRef,
    jval_ptr_type: LLVMTypeRef,
}

impl Module {
    /// Create a new CString associated with this LLVMModule,
    /// and return a pointer that can be passed to LLVM APIs.
    /// Assumes s is pure-ASCII.
    pub fn new_string_ptr(&mut self, s: &str) -> *const i8 {
        self.new_mut_string_ptr(s)
    }

    // TODO: ideally our pointers wouldn't be mutable.
    pub fn new_mut_string_ptr(&mut self, s: &str) -> *mut i8 {
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
        *self
            .global_scope_idents
            .entry(String::from(ident))
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
pub struct Builder {
    pub builder: *mut LLVMBuilder,
}

impl Builder {
    /// Create a new Builder in LLVM's global context.
    pub fn new() -> Self {
        unsafe {
            Builder {
                builder: LLVMCreateBuilder(),
            }
        }
    }

    pub fn position_at_end(&self, bb: LLVMBasicBlockRef) {
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

pub fn compile_to_module(
    module_name: &str,
    target_triple: Option<String>,
    do_report_mem_usage: bool,
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
                        parser::AstNode::GlobalVarAssgmt { ident: _, expr: _ } => (),
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
        let mut args = vec![int1(do_report_mem_usage as u64)];
        add_function_call(&mut module, main_bb, "jmemory_check", &mut args[..], "");

        add_main_cleanup(main_bb);

        module
    }
}

pub fn compile_expr(expr: &parser::AstNode, module: &mut Module, bb: LLVMBasicBlockRef) -> JValPtr {
    let builder = Builder::new();
    builder.position_at_end(bb);
    match *expr {
        parser::AstNode::Integer(n) => {
            unsafe {
                // Allocate space for the number.
                let num = LLVMBuildAlloca(
                    builder.builder,
                    int32_type(),
                    module.new_string_ptr("int_alloc"),
                );
                LLVMBuildStore(builder.builder, int32(n as u64), num);

                // Point to the number via a JVal struct.
                let ty = JValType::Integer;
                alloc_jval(
                    module,
                    bb,
                    num,
                    ty,
                    JValTypeParam::Numeric,
                    JValLocation::Stack,
                    vec![],
                )
            }
        }
        parser::AstNode::DoublePrecisionFloat(n) => {
            unsafe {
                // Allocate space for the number.
                let num = LLVMBuildAlloca(
                    builder.builder,
                    f64_type(),
                    module.new_string_ptr("dblfp_alloc"),
                );
                LLVMBuildStore(builder.builder, f64(n), num);

                // Point to the number via a JVal struct.
                alloc_jval(
                    module,
                    bb,
                    num,
                    JValType::DoublePrecisionFloat,
                    JValTypeParam::Numeric,
                    JValLocation::Stack,
                    vec![],
                )
            }
        }
        parser::AstNode::Str(ref str) => {
            unsafe {
                if str.len() == 1 {
                    // Single characters are scalars, not stored in an array.
                    // Allocate space for the character.
                    let char_alloc = LLVMBuildAlloca(
                        builder.builder,
                        int8_type(),
                        module.new_string_ptr("char_alloc"),
                    );
                    LLVMBuildStore(builder.builder, int8(str.as_bytes()[0] as u64), char_alloc);
                    // Point to the number via a JVal struct.
                    alloc_jval(
                        module,
                        bb,
                        char_alloc,
                        JValType::Character,
                        JValTypeParam::String,
                        JValLocation::Stack,
                        vec![],
                    )
                } else {
                    // Allocate an array to hold the string's characters.
                    let arr = LLVMBuildArrayAlloca(
                        builder.builder,
                        module.jval_ptr_type,
                        int64(str.as_bytes().len() as u64),
                        module.new_string_ptr("string_arr"),
                    );

                    // Load each character into the string array.
                    for (idx, byte) in str.as_bytes().iter().enumerate() {
                        // Allocate space for the character.
                        let char_alloc = LLVMBuildAlloca(
                            builder.builder,
                            int8_type(),
                            module.new_string_ptr("char_alloc"),
                        );
                        LLVMBuildStore(builder.builder, int8(*byte as u64), char_alloc);

                        // Point to the character via a JVal struct.
                        let char_jval = alloc_jval(
                            module,
                            bb,
                            char_alloc,
                            JValType::Character,
                            JValTypeParam::String,
                            JValLocation::Stack,
                            vec![],
                        );

                        // Store the JVal char at the appropriate offset in the array.
                        let mut char_offset = vec![int32(idx as u64)];
                        let char_gep = LLVMBuildInBoundsGEP(
                            builder.builder,
                            arr,
                            char_offset.as_mut_ptr(),
                            char_offset.len() as u32,
                            module.new_string_ptr("char_gep"),
                        );
                        LLVMBuildStore(builder.builder, char_jval.ptr, char_gep);
                    }
                    // Point to the string via a JVal struct.
                    alloc_jval(
                        module,
                        bb,
                        arr,
                        JValType::NDimensionalArray,
                        JValTypeParam::String,
                        JValLocation::Stack,
                        vec![str.len() as u64],
                    )
                }
            }
        }
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
                    module.new_string_ptr("terms_arr"),
                );

                // Load pointers to each JVal into the array.
                for (idx, jval) in compiled_terms.iter().enumerate() {
                    let mut term_offset = vec![int32(idx as u64)];
                    let term_gep = LLVMBuildInBoundsGEP(
                        builder.builder,
                        arr,
                        term_offset.as_mut_ptr(),
                        term_offset.len() as u32,
                        module.new_string_ptr("term_gep"),
                    );
                    LLVMBuildStore(builder.builder, jval.ptr, term_gep);
                }

                // Point to the array via a JVal struct.
                alloc_jval(
                    module,
                    bb,
                    arr,
                    JValType::NDimensionalArray,
                    JValTypeParam::Numeric,
                    JValLocation::Stack,
                    vec![compiled_terms.len() as u64],
                )
            }
        }
        parser::AstNode::MonadicOp { ref verb, ref expr } => {
            let expr = compile_expr(expr, module, bb);
            unsafe {
                let mut args = vec![int8(*verb as u64), expr.ptr];
                let monad_op_arr =
                    add_function_call(module, bb, "jmonad", &mut args[..], "monad_op_arr");

                // Drop the operand.
                let mut args = vec![expr.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");

                JValPtr { ptr: monad_op_arr }
            }
        }
        parser::AstNode::DyadicOp {
            ref verb,
            ref lhs,
            ref rhs,
        } => {
            let rhs = compile_expr(rhs, module, bb);
            let lhs = compile_expr(lhs, module, bb);

            // Pass args to dynamic library function; types/lengths will be resolved there.
            // TODO: If both type and len of lhs and rhs are statically known,
            // optimize by performing additions without function call overhead.
            unsafe {
                let mut args = vec![int8(*verb as u64), lhs.ptr, rhs.ptr];
                let dyad_op_arr =
                    add_function_call(module, bb, "jdyad", &mut args[..], "dyad_op_arr");

                // Drop the operands.
                let mut args = vec![lhs.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");
                let mut args = vec![rhs.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");

                JValPtr { ptr: dyad_op_arr }
            }
        }
        parser::AstNode::Reduce { ref verb, ref expr } => {
            let expr = compile_expr(expr, module, bb);

            // Pass args to dynamic library function; types/lengths will be resolved there.
            // TODO: If both type and len of lhs and rhs are statically known,
            // optimize by performing additions without function call overhead.
            unsafe {
                let mut args = vec![int8(*verb as u64), expr.ptr];
                let reduced_arr =
                    add_function_call(module, bb, "jreduce", &mut args[..], "reduced_arr");

                // Drop the operand.
                let mut args = vec![expr.ptr, int1(0)];
                add_function_call(module, bb, "jval_drop", &mut args[..], "");

                JValPtr { ptr: reduced_arr }
            }
        }
        parser::AstNode::GlobalVarAssgmt {
            ref ident,
            ref expr,
        } => {
            let expr = compile_expr(expr, module, bb);

            // IMPORTANT: For an assignment sequence such as:
            //   z =: 6
            //   z =: 8
            // this code will clone 8 into a global, then free the non-global 8,
            // and finally free 6. The issue is that
            // 6 could be referred to elsewhere, thus we eventually need reference counting.

            // Clone the JVal into the global heap space.
            let mut args = vec![expr.ptr, int64(JValLocation::HeapGlobal as u64)];
            let global_clone =
                unsafe { add_function_call(module, bb, "jval_clone", &mut args[..], "") };

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

            JValPtr { ptr: global_clone }
        }
        parser::AstNode::Ident(ref ident) => {
            let global_id = module.get_or_assign_id_to_global_ident(ident);
            unsafe {
                let mut args = vec![int32(global_id as u64)];
                let global_ref = add_function_call(
                    module,
                    bb,
                    "jglobal_get_reference",
                    &mut args[..],
                    "global_ref",
                );
                JValPtr { ptr: global_ref }
            }
        }
        _ => unimplemented!("Not ready to compile expr: {:?}", expr),
    }
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

    let jval_ptr_type = module.jval_ptr_type;
    let jval_ptr_ptr_type = unsafe { LLVMPointerType(module.jval_ptr_type, 0) };

    add_function(module, "jprint", &mut [jval_ptr_type, int1_type()], void);
    add_function(
        module,
        "jmonad",
        &mut [int8_type(), jval_ptr_type],
        jval_ptr_type,
    );
    add_function(
        module,
        "jdyad",
        &mut [int8_type(), jval_ptr_type, jval_ptr_type],
        jval_ptr_type,
    );
    add_function(
        module,
        "jreduce",
        &mut [int8_type(), jval_ptr_type],
        jval_ptr_type,
    );
    add_function(module, "jval_drop", &mut [jval_ptr_type, int1_type()], void);
    add_function(
        module,
        "jval_clone",
        &mut [jval_ptr_type, int8_type()],
        jval_ptr_type,
    );
    add_function(module, "jmemory_check", &mut [int1_type()], void);

    add_function(
        module,
        "jglobal_set_reference",
        &mut [int32_type(), jval_ptr_ptr_type],
        void,
    );
    add_function(
        module,
        "jglobal_get_reference",
        &mut [int32_type()],
        jval_ptr_type,
    );
    add_function(module, "jglobals_dropall", &mut [], void);
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

    let mut module = unsafe {
        let llvm_module = LLVMModuleCreateWithName(module_name_char_ptr);

        let global_ctx = LLVMGetGlobalContext();
        let jval_struct_type = LLVMStructCreateNamed(global_ctx, jval_struct_name_char_ptr);

        let jval_ptr_type = LLVMPointerType(jval_struct_type, 0);

        // IMPORTANT: Be sure this matches up with the corresponding definition
        // in jverbs.c.
        let mut members = vec![
            int8_type(),     // the value's type
            int8_type(),     // the value's type parameters
            int8_type(),     // the value's location
            int32_type(),    // the value's rank (number of dimensions)
            void_ptr_type(), // a pointer to the value
            jval_ptr_type,   // the value's shape (list of dimensions)
        ];
        LLVMStructSetBody(
            jval_struct_type,
            members.as_mut_ptr(),
            members.len() as u32,
            0,
        );

        let heap_counter_names = vec![
            "alive_heap_jval_counter",
            "total_heap_jval_counter",
            "alive_heap_int_counter",
            "total_heap_int_counter",
            "alive_heap_double_counter",
            "total_heap_double_counter",
            "alive_heap_char_counter",
            "total_heap_char_counter",
            "alive_heap_jvalptrarray_counter",
            "total_heap_jvalptrarray_counter",
        ];

        heap_counter_names.iter().for_each(|name| {
            let cstr_name = CString::new(*name).unwrap();
            let counter = LLVMAddGlobal(
                llvm_module,
                int32_type(),
                cstr_name.to_bytes_with_nul().as_ptr() as *const _,
            );
            LLVMSetInitializer(counter, int32(0));
            strings.push(cstr_name);
        });

        Module {
            module: llvm_module,
            strings,
            global_scope_idents: HashMap::new(),
            jval_struct_type,
            jval_ptr_type,
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
