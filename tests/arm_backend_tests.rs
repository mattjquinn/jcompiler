/// Integration tests for the ARM backend.
extern crate jcompilerlib;
extern crate tempfile;

use jcompilerlib::backend::arm::ARMBackend;
use std::process::Command;
use std::str;
use tempfile::NamedTempFile;

fn compile(test_jfile: &str) -> (String, String) {
    let unopt_compile_to_path = String::from(
        NamedTempFile::new()
            .unwrap()
            .path()
            .to_str()
            .expect("valid tempfile path"),
    );
    let backend = ARMBackend {};
    jcompilerlib::compile(
        &format!("jlang_programs/{}", test_jfile)[..],
        Box::new(backend),
        false,
        false,
        Some(unopt_compile_to_path.clone()),
    )
    .expect("unoptimized compilation failed");

    // NOTE: If you're on an amd_x64 system, running ARM binaries will only
    // be possible if you have "qemu-arm-static" installed (which registers
    // an interpreter that allows transparent ./ execution of ARM binaries).
    let unopt_output = Command::new(unopt_compile_to_path)
        .output()
        .expect("failed to execute unoptimized binary");
    let unopt_stdout = str::from_utf8(&unopt_output.stdout).unwrap().to_owned();
    let unopt_stderr = str::from_utf8(&unopt_output.stderr).unwrap().to_owned();

    // Return either set to caller for correctness assertions.
    (unopt_stdout, unopt_stderr)
}

#[test]
fn armtest_number_expr() {
    let (stdout, stderr) = compile("ctest_number_expr.ijs");
    assert_eq!("8\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_list_expr() {
    let (stdout, stderr) = compile("ctest_list_expr.ijs");
    assert_eq!(
        "2 4 6 8 10\n1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}
