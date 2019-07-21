/// Integration tests for the x86 backend.
extern crate jcompilerlib;
extern crate tempfile;

use std::process::Command;
use std::str;
use tempfile::NamedTempFile;
use jcompilerlib::backend::x86::X86Backend;

fn compile(test_jfile: &str) -> (String, String) {
    let unopt_compile_to_path = String::from(
        NamedTempFile::new()
            .unwrap()
            .path()
            .to_str()
            .expect("valid tempfile path"),
    );
    let backend = X86Backend {};
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

    // Return either set to caller for correctness assertions.
    (unopt_stdout, unopt_stderr)
}

//#[test]
//fn x86test_number_expr() {
//    let (stdout, stderr) = compile("ctest_number_expr.ijs");
//    assert_eq!("8\n", &stdout[..]);
//    assert_eq!("", &stderr[..]);
//}
