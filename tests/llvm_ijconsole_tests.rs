/// Integration tests for the LLVM backend.
extern crate jcompilerlib;
extern crate tempfile;
extern crate jcompiler_derive;

use jcompilerlib::backend::llvm::LLVMBackend;
use std::process::Command;
use std::str;
use tempfile::NamedTempFile;

mod common;

use jcompiler_derive::generate_tests;

fn compile(test_jfile: &str) -> (String, String) {
    // First compile *without* optimizations/stripping.
    let unopt_compile_to_path = String::from(
        NamedTempFile::new()
            .unwrap()
            .path()
            .to_str()
            .expect("valid tempfile path"),
    );
    let backend = LLVMBackend {
        target_triple: None,
        optimization_level: 0,
        do_strip_executable: false,
    };
    jcompilerlib::compile(
        &format!("jlang_programs/{}", test_jfile)[..],
        Box::new(backend),
        false,
        false,
        Some(unopt_compile_to_path.clone()),
    )
    .expect("unoptimized compilation failed");
    let unopt_output = Command::new(unopt_compile_to_path)
        .output()
        .expect("failed to execute unoptimized binary");
    let unopt_stdout = str::from_utf8(&unopt_output.stdout).unwrap().to_owned();
    let unopt_stderr = str::from_utf8(&unopt_output.stderr).unwrap().to_owned();

    // Then compile *with* optimizations/stripping.
    let opt_compile_to_path = String::from(
        NamedTempFile::new()
            .unwrap()
            .path()
            .to_str()
            .expect("valid tempfile path"),
    );
    let backend = LLVMBackend {
        target_triple: None,
        optimization_level: 3,
        do_strip_executable: true,
    };
    jcompilerlib::compile(
        &format!("jlang_programs/{}", test_jfile)[..],
        Box::new(backend),
        false,
        false,
        Some(opt_compile_to_path.clone()),
    )
    .expect("optimized/stripped compilation failed");
    let opt_output = Command::new(opt_compile_to_path)
        .output()
        .expect("failed to execute optimized/stripped binary");
    let opt_stdout = str::from_utf8(&opt_output.stdout).unwrap().to_owned();
    let opt_stderr = str::from_utf8(&opt_output.stderr).unwrap().to_owned();

    // Ensure outputs of both agree.
    assert_eq!(opt_stdout, unopt_stdout);
    assert_eq!(opt_stderr, unopt_stderr);

    // Return either set to caller for correctness assertions.
    (opt_stdout, opt_stderr)
}

generate_tests!([
    // Known test failures:
]);
