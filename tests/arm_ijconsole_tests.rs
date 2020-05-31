/// Integration tests for the ARM backend.
extern crate jcompilerlib;
extern crate tempfile;

use jcompilerlib::backend::arm::ARMBackend;
use std::process::{Command};
use std::str;
use tempfile::NamedTempFile;

mod common;

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

fn test(test_file: &str) {
    common::test(test_file, &compile);
}

#[test]
fn armtest_number_expr() {
    test("ctest_number_expr.ijs");
}

#[test]
fn armtest_list_expr() {
    test("ctest_list_expr.ijs");
}

#[test]
fn armtest_monadic_increment() {
    test("ctest_monadic_increment.ijs");
}

#[test]
fn armtest_double_monadic_increment() {
    test("ctest_double_monadic_increment.ijs");
}

#[test]
fn armtest_monadic_square() {
    test("ctest_monadic_square.ijs");
}

#[test]
fn armtest_double_monadic_square() {
    test("ctest_double_monadic_square.ijs");
}

#[test]
fn armtest_increment_square() {
    test("ctest_increment_square.ijs");
}

#[test]
fn armtest_additions_single_numbers() {
    test("ctest_additions_single_numbers.ijs");
}

#[test]
fn armtest_additions_lists() {
    test("ctest_additions_lists.ijs");
}

#[test]
fn armtest_products_single_numbers() {
    test("ctest_products_single_numbers.ijs");
}

#[test]
fn armtest_products_lists() {
    test("ctest_products_lists.ijs");
}

#[test]
fn armtest_mixed_adds_mults() {
    test("ctest_mixed_adds_mults.ijs");
}

#[test]
fn armtest_subtractions_single_positives() {
    test("ctest_subtractions_single_positives.ijs");
}

#[test]
fn armtest_subtractions_lists_positives() {
    test("ctest_subtractions_lists_positives.ijs");
}

#[test]
fn armtest_monadic_negate() {
    test("ctest_monadic_negate.ijs");
}

#[test]
fn armtest_additions_lists_mixedlens_legal() {
    test("ctest_additions_lists_mixedlens_legal.ijs");
}

#[test]
fn armtest_subtractions_lists_mixedlens_legal() {
    test("ctest_subtractions_lists_mixedlens_legal.ijs");
}

#[test]
fn armtest_products_lists_mixedlens_legal() {
    test("ctest_products_lists_mixedlens_legal.ijs");
}

#[test]
fn armtest_insertions_plus() {
    test("ctest_insertions_plus.ijs");
}

#[test]
fn armtest_insertions_times() {
    test("ctest_insertions_times.ijs");
}

#[test]
fn armtest_insertions_minus() {
    test("ctest_insertions_minus.ijs");
}

#[test]
fn armtest_lessthan() {
    test("ctest_lessthan.ijs");
}

#[test]
fn armtest_equal() {
    test("ctest_equal.ijs");
}

#[test]
fn armtest_largerthan() {
    test("ctest_largerthan.ijs");
}

#[test]
fn armtest_is_verb_globalassgmts() {
    test("ctest_is_verb_globalassgmts.ijs");
}

#[test]
fn armtest_global_assgmts_refs_integer() {
    test("ctest_global_assgmts_refs_integer.ijs");
}

#[test]
fn armtest_global_assgmts_refs_double() {
    test("ctest_global_assgmts_refs_double.ijs");
}

//#[test]
//fn armtest_global_assgmts_refs_array() {
//     test("ctest_global_assgmts_refs_array.ijs");
//}
