extern crate jcompiler_derive;
/// Integration tests for the ARM backend.
extern crate jcompilerlib;
extern crate tempfile;

use jcompilerlib::backend::arm::ARMBackend;
use std::process::Command;
use std::str;
use tempfile::NamedTempFile;

mod common;

use jcompiler_derive::generate_tests;

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

    // NOTE: Transparent execution of binaries on the Qemu ARM emulator
    // (i.e., using "./<name-of-binary>" is only possible if binfmt_misc
    // is configured, which will be done automatically on a raw machine
    // but not necessarily within a Docker container; to be safe we
    // always invoke the emulator directly.
    let unopt_output = Command::new("qemu-arm-static")
        .arg(unopt_compile_to_path)
        .output()
        .expect("failed to execute unoptimized binary");
    let unopt_stdout = str::from_utf8(&unopt_output.stdout).unwrap().to_owned();
    let unopt_stderr = str::from_utf8(&unopt_output.stderr).unwrap().to_owned();

    // Return either set to caller for correctness assertions.
    (unopt_stdout, unopt_stderr)
}

#[test]
fn next() {
    // common::test("netperf_ch1.ijs", &compile);
    // common::test("ctest_monadic_reciprocal.ijs", &compile);
}

generate_tests!([
    // Known test failures:
    "ctest_ch1_learningjbook.ijs",
    "ctest_ch2_learningjbook.ijs",
    "ctest_divisions.ijs",
    "ctest_dyadic_largerof.ijs",
    "ctest_dyadic_largerorequal.ijs",
    "ctest_dyadic_append.ijs",
    "ctest_dyadic_copy.ijs",
    "ctest_dyadic_shape.ijs",
    "ctest_global_refs_mixedverbs.ijs",
    "ctest_monadic_shapeof.ijs",
    "ctest_negative_numbers.ijs",
    "ctest_monadic_tally.ijs",
    "ctest_power.ijs",
    "ctest_strings.ijs",
    "j1.ijs"
]);
