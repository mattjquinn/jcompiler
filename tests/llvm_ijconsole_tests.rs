/// Integration tests for the LLVM backend.
extern crate jcompilerlib;
extern crate tempfile;

use jcompilerlib::backend::llvm::LLVMBackend;
use std::process::Command;
use std::str;
use tempfile::NamedTempFile;

mod common;

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

fn test(test_file: &str) {
    common::test(test_file, &compile);
}

#[test]
fn llvmtest_number_expr() {
    test("ctest_number_expr.ijs");
}

#[test]
fn llvmtest_list_expr() {
    test("ctest_list_expr.ijs");
}

#[test]
fn llvmtest_monadic_increment() {
    test("ctest_monadic_increment.ijs");
}

#[test]
fn llvmtest_double_monadic_increment() {
    test("ctest_double_monadic_increment.ijs");
}

#[test]
fn llvmtest_monadic_square() {
    test("ctest_monadic_square.ijs");
}

#[test]
fn llvmtest_double_monadic_square() {
    test("ctest_double_monadic_square.ijs");
}

#[test]
fn llvmtest_increment_square() {
    test("ctest_increment_square.ijs");
}

#[test]
fn llvmtest_additions_single_numbers() {
    test("ctest_additions_single_numbers.ijs");
}

#[test]
fn llvmtest_additions_lists() {
    test("ctest_additions_lists.ijs");
}

#[test]
fn llvmtest_products_single_numbers() {
    test("ctest_products_single_numbers.ijs");
}

#[test]
fn llvmtest_products_lists() {
    test("ctest_products_lists.ijs");
}

#[test]
fn llvmtest_mixed_adds_mults() {
    test("ctest_mixed_adds_mults.ijs");
}

#[test]
fn llvmtest_subtractions_single_positives() {
    test("ctest_subtractions_single_positives.ijs");
}

#[test]
fn llvmtest_subtractions_lists_positives() {
    test("ctest_subtractions_lists_positives.ijs");
}

#[test]
fn llvmtest_monadic_negate() {
    test("ctest_monadic_negate.ijs");
}

#[test]
fn llvmtest_additions_lists_mixedlens_legal() {
    test("ctest_additions_lists_mixedlens_legal.ijs");
}

#[test]
fn llvmtest_subtractions_lists_mixedlens_legal() {
    test("ctest_subtractions_lists_mixedlens_legal.ijs");
}

#[test]
fn llvmtest_products_lists_mixedlens_legal() {
    test("ctest_products_lists_mixedlens_legal.ijs");
}

#[test]
fn llvmtest_insertions_plus() {
    test("ctest_insertions_plus.ijs");
}

#[test]
fn llvmtest_insertions_times() {
    test("ctest_insertions_times.ijs");
}

#[test]
fn llvmtest_insertions_minus() {
    test("ctest_insertions_minus.ijs");
}

#[test]
fn llvmtest_lessthan() {
    test("ctest_lessthan.ijs");
}

#[test]
fn llvmtest_equal() {
    test("ctest_equal.ijs");
}

#[test]
fn llvmtest_largerthan() {
    test("ctest_largerthan.ijs");
}

#[test]
fn llvmtest_is_verb_globalassgmts() {
    test("ctest_is_verb_globalassgmts.ijs");
}

#[test]
fn llvmtest_global_assgmts_refs_integer() {
    test("ctest_global_assgmts_refs_integer.ijs");
}

#[test]
fn llvmtest_global_assgmts_refs_double() {
    test("ctest_global_assgmts_refs_double.ijs");
}

#[test]
fn llvmtest_global_assgmts_refs_array() {
    test("ctest_global_assgmts_refs_array.ijs");
}

#[test]
fn llvmtest_global_refs_mixedverbs() {
    test("ctest_global_refs_mixedverbs.ijs");
}

#[test]
fn llvmtest_negative_numbers() {
    test("ctest_negative_numbers.ijs");
}

#[test]
fn llvmtest_decimals() {
    test("ctest_decimals.ijs");
}

#[test]
fn llvmtest_divisions() {
    test("ctest_divisions.ijs");
}

#[test]
fn llvmtest_power() {
    test("ctest_power.ijs");
}

#[test]
fn llvmtest_monadic_reciprocal() {
    test("ctest_monadic_reciprocal.ijs");
}

#[test]
fn llvmtest_monadic_tally() {
    test("ctest_monadic_tally.ijs");
}

#[test]
fn llvmtest_dyadic_copy() {
    test("ctest_dyadic_copy.ijs");
}

#[test]
fn llvmtest_monadic_ceiling() {
    test("ctest_monadic_ceiling.ijs");
}

#[test]
fn llvmtest_monadic_largerof() {
    test("ctest_monadic_largerof.ijs");
}

#[test]
fn llvmtest_monadic_largerorequal() {
    test("ctest_monadic_largerorequal.ijs");
}

#[test]
fn llvmtest_ch1_learningjbook() {
    test("ctest_ch1_learningjbook.ijs");
}

#[test]
fn llvmtest_strings() {
    test("ctest_strings.ijs");
}

#[test]
fn llvmtest_dyadic_shape() {
    test("ctest_dyadic_shape.ijs");
}

#[test]
fn llvmtest_monadic_shapeof() {
    test("ctest_monadic_shapeof.ijs");
}

#[test]
fn llvmtest_ch2_learningjbook() {
    test("ctest_ch2_learningjbook.ijs");
}

#[test]
fn llvmtest_dyadic_append() {
    test("ctest_dyadic_append.ijs");
}
