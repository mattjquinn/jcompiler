/// Integration tests for the entire compiler.

extern crate jcompilerlib;
extern crate tempfile;

use std::str;
use tempfile::NamedTempFile;
use std::process::Command;

fn compile(test_jfile : &str) -> (String, String) {
    let compile_to_path = String::from(NamedTempFile::new().unwrap().path()
        .to_str().expect("valid tempfile path"));
    jcompilerlib::compile(&format!("jlang_programs/{}", test_jfile)[..],
                          None, Some(compile_to_path.clone()))
        .expect("compilation failed");
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

#[test]
fn ctest_monadic_square() {
    let (stdout, stderr) = compile("ctest_monadic_square.ijs");
    assert_eq!(&stdout[..], "1 4 9 16 25\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_double_monadic_square() {
    let (stdout, stderr) = compile("ctest_double_monadic_square.ijs");
    assert_eq!(&stdout[..], "1 16 81 256 625\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_increment_square() {
    let (stdout, stderr) = compile("ctest_increment_square.ijs");
    assert_eq!(&stdout[..], "2 5 10 17 26\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_additions_single_numbers() {
    let (stdout, stderr) = compile("ctest_additions_single_numbers.ijs");
    assert_eq!(&stdout[..], "3\n6\n10\n5\n0\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_additions_lists() {
    let (stdout, stderr) = compile("ctest_additions_lists.ijs");
    assert_eq!(&stdout[..], "3 3\n6 6 6\n12 15 14\n5\n0 0\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_products_single_numbers() {
    let (stdout, stderr) = compile("ctest_products_single_numbers.ijs");
    assert_eq!(&stdout[..], "2\n6\n24\n4\n0\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_products_lists() {
    let (stdout, stderr) = compile("ctest_products_lists.ijs");
    assert_eq!(&stdout[..], "2 2\n6 6 6\n18 0 70\n4\n0 0\n");
    assert_eq!(&stderr[..], "");
}

#[test]
fn ctest_mixed_adds_mults() {
    let (stdout, stderr) = compile("ctest_mixed_adds_mults.ijs");
    assert_eq!(&stdout[..], "170\n270\n45\n33\n7 7\n56 56\n28 28\n20 20 20\n10 10 10\n");
    assert_eq!(&stderr[..], "");
}