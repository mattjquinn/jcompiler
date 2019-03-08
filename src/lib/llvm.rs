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
const LLVM_TRUE: LLVMBool = 1;

pub fn compile_to_module(
    module_name: &str,
    target_triple: Option<String>,
    ast: &[parser::AstNode],
) -> Module {
    let mut module = create_module(module_name, target_triple);
    let main_fn = add_main_fn(&mut module);

    let (init_bb, mut bb) = add_initial_bbs(&mut module, main_fn);

    unsafe {
        let global_builder = Builder::new();
        global_builder.position_at_end(bb);

        // This is the point we want to start execution from.
        bb = set_entry_point_after(&mut module, main_fn, bb);

        let builder = Builder::new();
        builder.position_at_end(bb);

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut outputs = compile_expr(expr, &mut module, bb);
                    let printfmt_str =
                        compile_global_printfmt_str(outputs.len(), &mut module, &builder);
                    outputs.insert(0, printfmt_str);
                    add_function_call(&mut module, bb, "printf", &mut outputs, "");
                }
                _ => panic!("Not ready to copmile top-level AST node: {:?}", astnode),
            }
        }

        add_main_cleanup(bb);

        module
    }
}

fn compile_expr(
    expr: &parser::AstNode,
    module: &mut Module,
    bb: LLVMBasicBlockRef,
) -> Vec<LLVMValueRef> {
    match *expr {
        parser::AstNode::Number(n) => vec![compile_number_expr(n, module, bb)],
        //        parser::AstNode::BinAdd{ref lhs, ref rhs} =>
        //            compile_binadd_expr(&lhs, &rhs, module, bb),
        //        parser::AstNode::BinMul{ref lhs, ref rhs} =>
        //            compile_binmul_expr(&lhs, &rhs, module, bb),
        parser::AstNode::Terms(ref terms) => terms
            .iter()
            .flat_map(|t| compile_expr(t, module, bb))
            .collect_vec(),
        parser::AstNode::Increment(ref terms) => {
            let exprs = terms
                .iter()
                .flat_map(|e| compile_expr(e, module, bb))
                .collect_vec();
            exprs
                .into_iter()
                .map(|t| compile_increment(t, module, bb))
                .collect_vec()
        }
        _ => panic!("Not ready to compile expr: {:?}", expr),
    }
}

fn compile_number_expr(n: u32, module: &mut Module, bb: LLVMBasicBlockRef) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);

    unsafe {
        let n_alloc = LLVMBuildAlloca(builder.builder, int32_type(), module.new_string_ptr("n"));

        let n_init = int32(n as c_ulonglong);
        LLVMBuildStore(builder.builder, n_init, n_alloc);

        LLVMBuildLoad(builder.builder, n_alloc, module.new_string_ptr("n_load"))
    }
}

fn compile_increment(
    term: LLVMValueRef,
    module: &mut Module,
    bb: LLVMBasicBlockRef,
) -> LLVMValueRef {
    let builder = Builder::new();
    builder.position_at_end(bb);

    unsafe {
        LLVMBuildAdd(
            builder.builder,
            term,
            int32(1),
            module.new_string_ptr("increment"),
        )
    }
}

fn compile_global_printfmt_str(
    num_args: usize,
    module: &mut Module,
    builder: &Builder,
) -> LLVMValueRef {
    let str = (0..num_args).map(|_| "%d ").collect::<String>();
    let str = format!("{}\n", &str[..str.len() - 1]);

    unsafe {
        // TODO: Memoize these to prevent creation of identical format strings.
        LLVMBuildGlobalString(
            builder.builder,
            module.new_string_ptr(&str[..]),
            module.new_string_ptr(&format!("printfmt_{}_int_args", num_args)[..]),
        )
    }
}

//fn compile_binadd_expr(
//    a : &parser::AstNode,
//    b : &parser::AstNode,
//    module: &mut Module,
//    bb: LLVMBasicBlockRef) -> LLVMValueRef {
//
//    let builder = Builder::new();
//    builder.position_at_end(bb);
//
//    let aexp = compile_expr(a, module, bb);
//    let bexp = compile_expr(b, module, bb);
//
//    unsafe {
//        LLVMBuildAdd(
//            builder.builder,
//            aexp,
//            bexp,
//            module.new_string_ptr("sum"),
//        )
//    }
//}
//
//fn compile_binmul_expr(
//    a : &parser::AstNode,
//    b : &parser::AstNode,
//    module: &mut Module,
//    bb: LLVMBasicBlockRef) -> LLVMValueRef {
//
//    let builder = Builder::new();
//    builder.position_at_end(bb);
//
//    let aexp = compile_expr(a, module, bb);
//    let bexp = compile_expr(b, module, bb);
//
//    unsafe {
//        LLVMBuildMul(
//            builder.builder,
//            aexp,
//            bexp,
//            module.new_string_ptr("prod"),
//        )
//    }
//}

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
unsafe fn int8(val: c_ulonglong) -> LLVMValueRef {
    LLVMConstInt(LLVMInt8Type(), val, LLVM_FALSE)
}
/// Convert this integer to LLVM's representation of a constant
/// integer.
// TODO: this should be a machine word size rather than hard-coding 32-bits.
fn int32(val: c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt32Type(), val, LLVM_FALSE) }
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

fn int8_ptr_type() -> LLVMTypeRef {
    unsafe { LLVMPointerType(LLVMInt8Type(), 0) }
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

    add_function(
        module,
        "llvm.memset.p0i8.i32",
        &mut [
            int8_ptr_type(),
            int8_type(),
            int32_type(),
            int32_type(),
            int1_type(),
        ],
        void,
    );

    add_function(module, "malloc", &mut [int32_type()], int8_ptr_type());

    add_function(module, "free", &mut [int8_ptr_type()], void);

    add_function(
        module,
        "write",
        &mut [int32_type(), int8_ptr_type(), int32_type()],
        int32_type(),
    );

    add_function(module, "putchar", &mut [int32_type()], int32_type());

    add_function(module, "getchar", &mut [], int32_type());

    add_function(
        module,
        "printf",
        &mut [
            int8_ptr_type(), /* varargs; simply give extra args at call time. */
        ],
        int32_type(),
    );
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

/// Set up the initial basic blocks for appending instructions.
fn add_initial_bbs(
    module: &mut Module,
    main_fn: LLVMValueRef,
) -> (LLVMBasicBlockRef, LLVMBasicBlockRef) {
    unsafe {
        // This basic block is empty, but we will add a branch during
        // compilation according to InstrPosition.
        let init_bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr("init"));

        // We'll begin by appending instructions here.
        let beginning_bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr("beginning"));

        (init_bb, beginning_bb)
    }
}

/// Add prologue to main function.
unsafe fn add_main_cleanup(bb: LLVMBasicBlockRef) {
    let builder = Builder::new();
    builder.position_at_end(bb);

    let zero = int32(0);
    LLVMBuildRet(builder.builder, zero);
}

fn compile_static_outputs(module: &mut Module, bb: LLVMBasicBlockRef, outputs: &[i8]) {
    unsafe {
        let builder = Builder::new();
        builder.position_at_end(bb);

        let mut llvm_outputs = vec![];
        for value in outputs {
            llvm_outputs.push(int8(*value as c_ulonglong));
        }

        let output_buf_type = LLVMArrayType(int8_type(), llvm_outputs.len() as c_uint);
        let llvm_outputs_arr = LLVMConstArray(
            int8_type(),
            llvm_outputs.as_mut_ptr(),
            llvm_outputs.len() as c_uint,
        );

        let known_outputs = LLVMAddGlobal(
            module.module,
            output_buf_type,
            module.new_string_ptr("known_outputs"),
        );
        LLVMSetInitializer(known_outputs, llvm_outputs_arr);
        LLVMSetGlobalConstant(known_outputs, LLVM_TRUE);

        let stdout_fd = int32(1);
        let llvm_num_outputs = int32(outputs.len() as c_ulonglong);

        let known_outputs_ptr = LLVMBuildPointerCast(
            builder.builder,
            known_outputs,
            int8_ptr_type(),
            module.new_string_ptr("known_outputs_ptr"),
        );

        add_function_call(
            module,
            bb,
            "write",
            &mut [stdout_fd, known_outputs_ptr, llvm_num_outputs],
            "",
        );
    }
}

/// Ensure that execution starts after the basic block we pass in.
unsafe fn set_entry_point_after(
    module: &mut Module,
    main_fn: LLVMValueRef,
    bb: LLVMBasicBlockRef,
) -> LLVMBasicBlockRef {
    let after_init_bb = LLVMAppendBasicBlock(main_fn, module.new_string_ptr("after_init"));

    // From the current bb, we want to continue execution in after_init.
    let builder = Builder::new();
    builder.position_at_end(bb);
    LLVMBuildBr(builder.builder, after_init_bb);

    // We also want to start execution in after_init.
    let init_bb = LLVMGetFirstBasicBlock(main_fn);
    builder.position_at_end(init_bb);
    LLVMBuildBr(builder.builder, after_init_bb);

    after_init_bb
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