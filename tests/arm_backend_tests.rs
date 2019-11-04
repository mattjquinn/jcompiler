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

#[test]
fn armtest_monadic_increment() {
    let (stdout, stderr) = compile("ctest_monadic_increment.ijs");
    assert_eq!("2 3 4 5 6\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_double_monadic_increment() {
    let (stdout, stderr) = compile("ctest_double_monadic_increment.ijs");
    assert_eq!("107 2002 70 46 90 2\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_monadic_square() {
    let (stdout, stderr) = compile("ctest_monadic_square.ijs");
    assert_eq!("1 4 9 16 25\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_double_monadic_square() {
    let (stdout, stderr) = compile("ctest_double_monadic_square.ijs");
    assert_eq!("1 16 81 256 625\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_increment_square() {
    let (stdout, stderr) = compile("ctest_increment_square.ijs");
    assert_eq!("2 5 10 17 26\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_additions_single_numbers() {
    let (stdout, stderr) = compile("ctest_additions_single_numbers.ijs");
    assert_eq!("3\n6\n10\n5\n0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_additions_lists() {
    let (stdout, stderr) = compile("ctest_additions_lists.ijs");
    assert_eq!("3 3\n6 6 6\n12 15 14\n5\n0 0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_products_single_numbers() {
    let (stdout, stderr) = compile("ctest_products_single_numbers.ijs");
    assert_eq!("2\n6\n24\n4\n0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_products_lists() {
    let (stdout, stderr) = compile("ctest_products_lists.ijs");
    assert_eq!("2 2\n6 6 6\n18 0 70\n4\n0 0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_mixed_adds_mults() {
    let (stdout, stderr) = compile("ctest_mixed_adds_mults.ijs");
    assert_eq!(
        "170\n270\n45\n33\n7 7\n56 56\n28 28\n20 20 20\n10 10 10\n_2969 _3719\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_subtractions_single_positives() {
    let (stdout, stderr) = compile("ctest_subtractions_single_positives.ijs");
    assert_eq!("_1\n2\n_2\n3\n0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_subtractions_lists_positives() {
    let (stdout, stderr) = compile("ctest_subtractions_lists_positives.ijs");
    assert_eq!("_1 _1\n2 2 2\n8 _3 10\n0 0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_monadic_negate() {
    let (stdout, stderr) = compile("ctest_monadic_negate.ijs");
    assert_eq!(
        "_5\n6\n_7\n8\n_2\n_1 _2 _3\n_5 _4\n2 2\n2\n2\n2\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_additions_lists_mixedlens_legal() {
    let (stdout, stderr) = compile("ctest_additions_lists_mixedlens_legal.ijs");
    assert_eq!(
        "11 21 31\n11 21 31\n8 9\n7 8\n6 7\n0 0 0 0\n0 0 0\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_subtractions_lists_mixedlens_legal() {
    let (stdout, stderr) = compile("ctest_subtractions_lists_mixedlens_legal.ijs");
    assert_eq!(
        "_9 _19 _29\n9 19 29\n2 3\n3 2\n2 3\n0 0 0 0\n0 0 0\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_products_lists_mixedlens_legal() {
    let (stdout, stderr) = compile("ctest_products_lists_mixedlens_legal.ijs");
    assert_eq!(
        "40 80 120\n40 80 120\n12 24\n8 12\n6 8\n0 0 0 0\n0 0 0\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_insertions_plus() {
    let (stdout, stderr) = compile("ctest_insertions_plus.ijs");
    assert_eq!("5\n3\n6\n11\n2\n_26\n_26\n_10\n90 180 270\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_insertions_times() {
    let (stdout, stderr) = compile("ctest_insertions_times.ijs");
    assert_eq!(
        "0\n5\n1\n2\n6\n30\n2\n_332\n5\n_10\n34 44 54\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_insertions_minus() {
    let (stdout, stderr) = compile("ctest_insertions_minus.ijs");
    assert_eq!("0\n5\n1\n_1\n2\n3\n16\n_3\n14\n10\n13 23 33\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_lessthan() {
    let (stdout, stderr) = compile("ctest_lessthan.ijs");
    assert_eq!(
        "0\n1\n0 0 0 1\n1 1 0 0\n0 1 0 1\n0 0 0 0 0 1 1 1 1 1 1 1 1\n1 1 1 1 0 0 0 0 0 0 0 0 0\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_equal() {
    let (stdout, stderr) = compile("ctest_equal.ijs");
    assert_eq!("0\n1\n0 0 1\n1 1 1\n0 0 0\n1 1 1\n0 0 0 0 1 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 1 0 0 0 0 0 0 0 0 0 0\n", &stdout[..]);
    assert_eq!("", &stderr[..]);
}

#[test]
fn armtest_largerthan() {
    let (stdout, stderr) = compile("ctest_largerthan.ijs");
    assert_eq!(
        "1\n0\n1 1 0 0\n0 0 0 1\n0 0 1 0\n1 1 1 1 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 1 1 1 1 1 1 1 1\n",
        &stdout[..]
    );
    assert_eq!("", &stderr[..]);
}
