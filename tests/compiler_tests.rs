/// Integration tests for the entire compiler.

extern crate jcompilerlib;
extern crate tempfile;

use std::{fs, str};
use tempfile::NamedTempFile;
use std::process::Command;

fn compile(test_jfile : &str) -> (String, String) {
    let compile_to_path = String::from(NamedTempFile::new().unwrap().path()
        .to_str().expect("valid tempfile path"));
    jcompilerlib::compile(&format!("jlang_programs/{}", test_jfile)[..],
                          None, Some(compile_to_path.clone()));
    let output = Command::new(compile_to_path)
        .output()
        .expect("failed to execute compiled binary");
    let stdout = str::from_utf8(&output.stdout).unwrap().to_owned();
    let stderr = str::from_utf8(&output.stderr).unwrap().to_owned();
    (stdout, stderr)
}

#[test]
fn ctest_number_expr() {
    let (stdout, stderr) = compile("ctest_number_expr.ijs");
    assert_eq!(&stdout[..], "8\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_list_expr() {
    let (stdout, stderr) = compile("ctest_list_expr.ijs");
    assert_eq!(&stdout[..], "2 4 6 8 10\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_monadic_increment() {
    let (stdout, stderr) = compile("ctest_monadic_increment.ijs");
    assert_eq!(&stdout[..], "2 3 4 5 6\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_double_monadic_increment() {
    let (stdout, stderr) = compile("ctest_double_monadic_increment.ijs");
    assert_eq!(&stdout[..], "107 2002 70 46 90 2\n");
    assert_eq!(&stderr[..], "");
}
