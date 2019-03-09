/// Integration tests for the entire compiler.

extern crate jcompilerlib;
extern crate tempfile;

use std::{fs, str};
use tempfile::NamedTempFile;
use std::process::Command;

#[test]
fn compiler_test_number_expr() {
    let compile_to_path = String::from(NamedTempFile::new().unwrap().path()
                                          .to_str().expect("valid tempfile path"));
    jcompilerlib::compile("jlang_programs/ctest_number_expr.ijs", None, Some(compile_to_path.clone()));
    let output = Command::new(compile_to_path)
        .output()
        .expect("failed to execute compiled binary");
    assert_eq!(str::from_utf8(&output.stdout).unwrap(), "8\n");
}