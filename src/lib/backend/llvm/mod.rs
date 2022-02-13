mod compiler;
mod runtime;
mod support;

use getopts::{Matches, Options};
use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

use tempfile::NamedTempFile;

use std::ffi::{CStr, CString};
use std::ptr::null_mut;
use std::str;

use backend;
use parser;
use shell;

use self::compiler::{compile_to_module, optimise_ir, Module};

pub struct LLVMBackend {
    pub target_triple: Option<String>,
    pub optimization_level: u8,
    pub do_strip_executable: bool,
}

impl ::Backend for LLVMBackend {
    fn compile_ast(
        &self,
        path: &str,
        ast: &Vec<parser::AstNode>,
        do_report_mem_usage: bool,
        do_verbose: bool,
        output_path: String,
    ) -> Result<(), String> {
        let mut llvm_module =
            compile_to_module(path, self.target_triple.clone(), do_report_mem_usage, &ast);

        optimise_ir(&mut llvm_module, self.optimization_level as i64);
        let llvm_ir_cstr = llvm_module.to_cstring();
        let llvm_ir = String::from_utf8_lossy(llvm_ir_cstr.as_bytes());

        if do_verbose {
            println!(
                "LLVM IR optimized at level {}:\n{}",
                self.optimization_level, llvm_ir
            );
        }

        // Compile the LLVM IR to a temporary object file.
        let object_file = backend::convert_io_error(NamedTempFile::new())?;
        let obj_file_path = object_file.path().to_str().expect("path not valid utf-8");

        if do_verbose {
            println!("Writing object file to {}", obj_file_path);
        }

        write_object_file(&mut llvm_module, &obj_file_path).unwrap();

        if do_verbose {
            println!("Writing executable to {}", output_path);
        }

        let res = link_object_file(&obj_file_path, &output_path, self.target_triple.clone());
        match res {
            Ok(_) => (),
            Err(e) => panic!("Linking executable failed: {}", e),
        };

        if self.do_strip_executable {
            let strip_args = ["-s", &output_path[..]];
            shell::run_shell_command("strip", &strip_args[..]).unwrap();
            if do_verbose {
                println!("Stripped executable of debug symbols.");
            }
        }

        Ok(())
    }
}

pub fn register_cli_options(options: &mut Options) {
    let default_triple_cstring = get_default_target_triple();
    let default_triple = default_triple_cstring.to_str().unwrap();

    options.optopt(
        "",
        "llvm-target",
        &format!(
            "if using LLVM backend, specifies the LLVM target triple (default: {})",
            default_triple
        ),
        "TARGET",
    );
    options.optopt(
        "",
        "llvm-opt",
        "if using LLVM backend, sets the LLVM optimization level (0 to 3)",
        "LVL",
    );
    options.optopt(
        "",
        "llvm-strip",
        "if using LLVM backend, strips symbols from the binary (default: yes)",
        "yes|no",
    );
}

pub fn init_from_cli_options(matches: &Matches) -> Result<Box<dyn(::Backend)>, String> {
    let target_triple = matches.opt_str("llvm-target");
    let optimization_level: u8 = match matches.opt_str("llvm-opt") {
        Some(lvlstr) => match lvlstr.parse::<u8>() {
            Ok(n) if n <= 3 => n,
            _ => {
                return Err(format!(
                    "Unrecognized choice \"{}\" for --llvm-opt; need \"0\", \"1\", \"2\", or \"3\".",
                    lvlstr
                ));
            }
        },
        _ => 0,
    };

    let do_strip_executable = match matches.opt_str("llvm-strip") {
        Some(ans) => match &ans[..] {
            "yes" => true,
            "no" => false,
            _ => {
                return Err(format!(
                    "Unrecognized choice \"{}\" for --llvm-strip; need \"yes\" or \"no\".",
                    ans
                ));
            }
        },
        _ => true, // Strip executables of debugging symbols by default.
    };
    Ok(Box::new(LLVMBackend {
        target_triple,
        optimization_level,
        do_strip_executable,
    }))
}

fn link_object_file(
    object_file_path: &str,
    executable_path: &str,
    target_triple: Option<String>,
) -> Result<(), String> {
    // Link the object file.
    let clang_args = if let Some(ref target_triple) = target_triple {
        vec![
            object_file_path,
            "c_defns/jverbs.c",
            "c_defns/jmemory.c",
            "-target",
            &target_triple,
            "-o",
            &executable_path[..],
            "-lm",
        ]
    } else {
        vec![
            object_file_path,
            "c_defns/jverbs.c",
            "c_defns/jmemory.c",
            "-o",
            &executable_path[..],
            "-lm",
        ]
    };

    shell::run_shell_command("clang-10", &clang_args[..])
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
