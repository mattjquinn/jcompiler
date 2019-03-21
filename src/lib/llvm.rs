use itertools::Itertools;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::transforms::pass_manager_builder::*;
use llvm_sys::{LLVMBuilder, LLVMModule};

use std::ffi::{CStr, CString};
use std::os::raw::{c_uint, c_ulonglong};
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

    let main_fn = add_main_fn(&mut module);

    unsafe {
        let bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr(""));
        let builder = Builder::new();
        builder.position_at_end(bb);

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let expr = compile_expr(expr, &mut module, bb);
                    let mut args = vec![expr.ptr, int32(expr.arr_len as u64)];
                    add_function_call(&mut module, bb, "jprint", &mut args[..], "");
                }
                _ => panic!("Not ready to compile top-level AST node: {:?}", astnode),
            }
        }

        add_main_cleanup(bb);

        module
    }
}

struct ExprArrayPtr {
    ptr: LLVMValueRef,
    arr_len: u32,
}

fn compile_expr(
    expr: &parser::AstNode,
    module: &mut Module,
    bb: LLVMBasicBlockRef,
) -> ExprArrayPtr {
    match *expr {
        parser::AstNode::Number(n) => {
            let mut builder = Builder::new();
            builder.position_at_end(bb);
            unsafe {
                let atype = LLVMArrayType(
                    int32_type(),
                    1
                );
                let arr = LLVMBuildArrayAlloca(
                    builder.builder,
                    atype,
                    int32(0),
                    module.new_string_ptr("arr")
                );
                LLVMSetAlignment(arr, 16);
                let mut offset_vec = vec![int64(0), int64(0)];
                let gep = LLVMBuildGEP(
                    builder.builder,
                    arr,
                    offset_vec.as_mut_ptr(),
                    offset_vec.len() as u32,
                    module.new_string_ptr("arridx"),
                );
                LLVMBuildStore(builder.builder, int32(n as u64), gep);
                ExprArrayPtr {ptr: arr, arr_len: 1 }
            }
        },
        parser::AstNode::Terms(ref terms) => {
            let compiled_terms = terms
                .iter()
                .map(|t| compile_expr(t, module, bb))
                .collect_vec();
            if compiled_terms.len() == 1 {
                // If there is only one term in this terms list, simply
                // reuse the array representing that term.
                return ExprArrayPtr { ptr : compiled_terms[0].ptr, arr_len : compiled_terms[0].arr_len};
            }
            unsafe {
                // If there is more than one term, combine each term's array
                // representation into a single array.
                for c in compiled_terms.iter() {
                    // For now, only combine subterms that have a single element;
                    // merging generally requires more thinking about how that
                    // works / is represented in J.
                    assert_eq!(c.arr_len, 1);
                }
                let mut builder = Builder::new();
                builder.position_at_end(bb);
                let atype = LLVMArrayType(
                    int32_type(),
                    compiled_terms.len() as u32
                );
                let arr = LLVMBuildArrayAlloca(
                    builder.builder,
                    atype,
                    int32(0),
                    module.new_string_ptr("arr")
                );
                LLVMSetAlignment(arr, 16);
                for (idx, t) in compiled_terms.iter().enumerate() {
                    let mut dest_offset_vec = vec![int64(0), int64(idx as c_ulonglong)];
                    let dest_gep = LLVMBuildGEP(
                        builder.builder,
                        arr,
                        dest_offset_vec.as_mut_ptr(),
                        dest_offset_vec.len() as u32,
                        module.new_string_ptr("dest_arr"),
                    );
                    // Using offset of 0 per assumption of only one element above.
                    let mut src_offset_vec = vec![int64(0), int64(0)];
                    let src_gep = LLVMBuildGEP(
                        builder.builder,
                        t.ptr,
                        src_offset_vec.as_mut_ptr(),
                        src_offset_vec.len() as u32,
                        module.new_string_ptr("src_arr"),
                    );
                    let src_load = LLVMBuildLoad(
                        builder.builder,
                        src_gep,
                        module.new_string_ptr("src_load")
                    );
                    LLVMBuildStore(builder.builder, src_load, dest_gep);
                }
                ExprArrayPtr {ptr: arr, arr_len: compiled_terms.len() as u32 }
            }
        },
        parser::AstNode::Increment(ref terms) => {
            let exprs = compile_expr(terms, module, bb);
            unsafe {
                let mut args = vec![exprs.ptr, int32(exprs.arr_len as u64)];
                let inc_arr = add_function_call(module, bb, "jincrement", &mut args[..], "inc_arr");
                ExprArrayPtr { ptr : inc_arr, arr_len: exprs.arr_len }
            }
        },
        parser::AstNode::Square(ref terms) => {
            let exprs = compile_expr(terms, module, bb);
            unsafe {
                let mut args = vec![exprs.ptr, int32(exprs.arr_len as u64)];
                let sq_arr = add_function_call(module, bb, "jsquare", &mut args[..], "sq_arr");
                ExprArrayPtr { ptr : sq_arr, arr_len: exprs.arr_len }
            }
        },
        parser::AstNode::Negate(ref terms) => {
            let exprs = compile_expr(terms, module, bb);
            unsafe {
                let mut args = vec![exprs.ptr, int32(exprs.arr_len as u64)];
                let neg_arr = add_function_call(module, bb, "jnegate", &mut args[..], "neg_arr");
                ExprArrayPtr { ptr : neg_arr, arr_len: exprs.arr_len }
            }
        },
        parser::AstNode::Plus{ref lhs, ref rhs} => {
            let mut rhs = compile_expr(rhs, module, bb);
            let mut lhs = compile_expr(lhs, module, bb);

            // If either of the arguments is shorter than the other,
            // we must expand it before passing it to the addition verb.
            let res_length = std::cmp::max(lhs.arr_len, rhs.arr_len);
            if lhs.arr_len != res_length {
                lhs = expand_expr_array(&lhs, res_length, module, bb);
            }
            if rhs.arr_len != res_length {
                rhs = expand_expr_array(&rhs, res_length, module, bb);
            }

            unsafe {
                let mut args = vec![lhs.ptr, int32(lhs.arr_len as u64), rhs.ptr, int32(rhs.arr_len as u64)];
                let plus_arr = add_function_call(module, bb, "jplus", &mut args[..], "plus_arr");
                ExprArrayPtr { ptr : plus_arr, arr_len: res_length }
            }
        },
        parser::AstNode::Minus{ref lhs, ref rhs} => {
            let mut rhs = compile_expr(rhs, module, bb);
            let mut lhs = compile_expr(lhs, module, bb);

            // If either of the arguments is shorter than the other,
            // we must expand it before passing it to the subtraction verb.
            let res_length = std::cmp::max(lhs.arr_len, rhs.arr_len);
            if lhs.arr_len != res_length {
                lhs = expand_expr_array(&lhs, res_length, module, bb);
            }
            if rhs.arr_len != res_length {
                rhs = expand_expr_array(&rhs, res_length, module, bb);
            }

            unsafe {
                let mut args = vec![lhs.ptr, int32(lhs.arr_len as u64), rhs.ptr, int32(rhs.arr_len as u64)];
                let sub_arr = add_function_call(module, bb, "jminus", &mut args[..], "minus_arr");
                ExprArrayPtr { ptr : sub_arr, arr_len: res_length }
            }
        },
        parser::AstNode::Times{ref lhs, ref rhs} => {
            let mut rhs = compile_expr(rhs, module, bb);
            let mut lhs = compile_expr(lhs, module, bb);

            // If either of the arguments is shorter than the other,
            // we must expand it before passing it to the times verb.
            let res_length = std::cmp::max(lhs.arr_len, rhs.arr_len);
            if lhs.arr_len != res_length {
                lhs = expand_expr_array(&lhs, res_length, module, bb);
            }
            if rhs.arr_len != res_length {
                rhs = expand_expr_array(&rhs, res_length, module, bb);
            }

            unsafe {
                let mut args = vec![lhs.ptr, int32(lhs.arr_len as u64), rhs.ptr, int32(rhs.arr_len as u64)];
                let plus_arr = add_function_call(module, bb, "jtimes", &mut args[..], "times_arr");
                ExprArrayPtr { ptr : plus_arr, arr_len: res_length }
            }
        },
        parser::AstNode::Reduce{ ref dyadic_verb, ref expr } => {
            let mut expr = compile_expr(expr, module, bb);
            let mut args = vec![expr.ptr, int32(expr.arr_len as u64)];
            let reduced_arr = match &dyadic_verb[..] {
                "+" => {
                    unsafe {
                        add_function_call(
                            module, bb, "jreduce_plus", &mut args[..], "reduced_plus_arr")
                    }
                },
                "*" => {
                    unsafe {
                        add_function_call(
                            module, bb, "jreduce_times", &mut args[..], "reduced_times_arr")
                    }
                },
                "-" => {
                    unsafe {
                        add_function_call(
                            module, bb, "jreduce_minus", &mut args[..], "reduced_minus_arr")
                    }
                },
                _ => { unimplemented!("Not ready to compile insert (reduce) with dyad: {}", dyadic_verb) }
            };
            ExprArrayPtr { ptr : reduced_arr, arr_len: 1 }
        },
        parser::AstNode::LessThan{ref lhs, ref rhs} => {
            let mut rhs = compile_expr(rhs, module, bb);
            let mut lhs = compile_expr(lhs, module, bb);

            // If either of the arguments is shorter than the other,
            // we must expand it before passing it to the times verb.
            let res_length = std::cmp::max(lhs.arr_len, rhs.arr_len);
            if lhs.arr_len != res_length {
                lhs = expand_expr_array(&lhs, res_length, module, bb);
            }
            if rhs.arr_len != res_length {
                rhs = expand_expr_array(&rhs, res_length, module, bb);
            }

            unsafe {
                let mut args = vec![lhs.ptr, int32(lhs.arr_len as u64), rhs.ptr, int32(rhs.arr_len as u64)];
                let lt_arr = add_function_call(module, bb, "jlessthan", &mut args[..], "lessthan_arr");
                ExprArrayPtr { ptr : lt_arr, arr_len: res_length }
            }
        },
        _ => unimplemented!("Not ready to compile expr: {:?}", expr),
    }
}

fn expand_expr_array(expr_array : &ExprArrayPtr,
                     to_len: u32,
                     module: &mut Module,
                     bb: LLVMBasicBlockRef) -> ExprArrayPtr {
    if expr_array.arr_len != 1 {
        unimplemented!("Can only expand arrays of size 1 now; this one is: {}", expr_array.arr_len);
    }
    unsafe {
        let builder = Builder::new();
        builder.position_at_end(bb);
        let atype = LLVMArrayType(
            int32_type(),
            to_len
        );
        let arr = LLVMBuildArrayAlloca(
            builder.builder,
            atype,
            int32(0),
            module.new_string_ptr("expansion_arr")
        );
        LLVMSetAlignment(arr, 16);
        // Load the sole element out of the unexpanded array;
        // using offset of 0 per assumption of only one element above.
        let fromtype = LLVMArrayType(
            int32_type(),
            1,
        );

        // IMPORTANT: Arrays allocated in the C funcs are simply pointers to
        // an int array; we must cast to the array type here in order to index into it...
        let expr_arr_ptr_cast = LLVMBuildPointerCast(
            builder.builder,
            expr_array.ptr,
            fromtype,
            module.new_string_ptr("prexpand_src_ptr_cast"));
        // ...and because we are not casting to a pointer (just to an array), we do
        // NOT need the first 0 index here to deref a pointer; just the second
        // to index into the (first) position of the array.
        let mut src_offset_vec = vec![int64(0)];

        let src_gep = LLVMBuildGEP(
            builder.builder,
            expr_arr_ptr_cast,
            src_offset_vec.as_mut_ptr(),
            src_offset_vec.len() as u32,
            module.new_string_ptr("prexpand_src_arr"),
        );
        let src_load = LLVMBuildLoad(
            builder.builder,
            src_gep,
            module.new_string_ptr("prexpand_src_load")
        );
        for idx in 0..to_len {
            let mut dest_offset_vec = vec![int64(0), int64(idx as c_ulonglong)];
            let dest_gep = LLVMBuildGEP(
                builder.builder,
                arr,
                dest_offset_vec.as_mut_ptr(),
                dest_offset_vec.len() as u32,
                module.new_string_ptr("postexpand_dest_arr"),
            );
            LLVMBuildStore(builder.builder, src_load, dest_gep);
        }
        ExprArrayPtr { ptr : arr, arr_len : to_len }
    }
}

/// A struct that keeps ownership of all the strings we've passed to
/// the LLVM API until we destroy the `LLVMModule`.
pub struct Module {
    module: *mut LLVMModule,
    strings: Vec<CString>,
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

/// Convert this integer to LLVM's representation of a constant
/// integer.
//unsafe fn int8(val: c_ulonglong) -> LLVMValueRef {
//    LLVMConstInt(LLVMInt8Type(), val, LLVM_FALSE)
//}
/// Convert this integer to LLVM's representation of a constant
/// integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
fn int32(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt32Type(), val, LLVM_FALSE) }
}

fn int64(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt64Type(), val, LLVM_FALSE) }
}

//fn int1_type() -> LLVMTypeRef {
//    unsafe { LLVMInt1Type() }
//}
//
//fn int8_type() -> LLVMTypeRef {
//    unsafe { LLVMInt8Type() }
//}

fn int32_type() -> LLVMTypeRef {
    unsafe { LLVMInt32Type() }
}

//fn int8_ptr_type() -> LLVMTypeRef {
//    unsafe { LLVMPointerType(LLVMInt8Type(), 0) }
//}

fn int32_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMInt32Type(), 0) }
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

    add_function(module, "jprint", &mut [int32_ptr_type(), int32_type()], void);
    add_function(module, "jnegate", &mut [int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jincrement", &mut [int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jsquare", &mut [int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jplus", &mut [int32_ptr_type(), int32_type(), int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jminus", &mut [int32_ptr_type(), int32_type(), int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jtimes", &mut [int32_ptr_type(), int32_type(), int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jreduce_plus", &mut [int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jreduce_times", &mut [int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jreduce_minus", &mut [int32_ptr_type(), int32_type()], int32_ptr_type());
    add_function(module, "jlessthan", &mut [int32_ptr_type(), int32_type(), int32_ptr_type(), int32_type()], int32_ptr_type());
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
    let c_module_name = CString::new(module_name).unwrap();
    let module_name_char_ptr = c_module_name.to_bytes_with_nul().as_ptr() as *const _;

    let llvm_module;
    unsafe {
        llvm_module = LLVMModuleCreateWithName(module_name_char_ptr);
    }
    let mut module = Module {
        module: llvm_module,
        strings: vec![c_module_name],
    };

    let target_triple_cstring = if let Some(target_triple) = target_triple {
        CString::new(target_triple).unwrap()
    } else {
        get_default_target_triple()
    };

    // This is necessary for maximum LLVM performance, see
    // http://llvm.org/docs/Frontend/PerformanceTips.html
    unsafe {
        LLVMSetTarget(llvm_module, target_triple_cstring.as_ptr() as *const _);
    }
    // TODO: add a function to the LLVM C API that gives us the
    // data layout from the target machine.

    add_c_declarations(&mut module);
    module
}

fn add_main_fn(module: &mut Module) -> LLVMValueRef {
    let mut main_args = vec![];
    unsafe {
        let main_type = LLVMFunctionType(int32_type(), main_args.as_mut_ptr(), 0, LLVM_FALSE);
        // TODO: use add_function() here instead.
        LLVMAddFunction(module.module, module.new_string_ptr("main"), main_type)
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
