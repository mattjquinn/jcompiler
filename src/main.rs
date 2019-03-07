//! jcompiler is a compiler for the J programming language.

extern crate ansi_term;
extern crate getopts;
extern crate itertools;
extern crate jcompilerlib;
extern crate llvm_sys;
extern crate matches;
extern crate quickcheck;
extern crate rand;
extern crate regex;
extern crate tempfile;

use getopts::{Matches, Options};
use jcompilerlib::{llvm, parser, shell};
use std::env;
use std::fs;
use std::path::Path;
use tempfile::NamedTempFile;

/// Convert "foo.ijs" to "foo".
fn executable_name(ijs_path: &str) -> String {
    let bf_file_name = Path::new(ijs_path).file_name().unwrap().to_str().unwrap();

    let mut name_parts: Vec<_> = bf_file_name.split('.').collect();
    let parts_len = name_parts.len();
    if parts_len > 1 {
        name_parts.pop();
    }

    name_parts.join(".")
}

fn print_usage(bin_name: &str, opts: Options) {
    let brief = format!("Usage: {} SOURCE_FILE [options]", bin_name);
    print!("{}", opts.usage(&brief));
}

fn convert_io_error<T>(result: Result<T, std::io::Error>) -> Result<T, String> {
    match result {
        Ok(value) => Ok(value),
        Err(e) => Err(format!("{}", e)),
    }
}

fn compile_jlang_file(matches: &Matches) -> Result<(), String> {
    let path = &matches.free[0];

    let jsrc = fs::read_to_string(path).expect("cannot open source of provided J program");

    let ast = match parser::parse(&jsrc[..]) {
        Ok(instrs) => instrs,
        Err(parse_error) => {
            panic!("{}", parse_error);
        }
    };
    for astnode in &ast {
        println!("{:?}", astnode);
    }

    let target_triple = matches.opt_str("target");
    let mut llvm_module = llvm::compile_to_module(path, target_triple.clone(), &ast);

    let llvm_ir_cstr = llvm_module.to_cstring();
    let llvm_ir = String::from_utf8_lossy(llvm_ir_cstr.as_bytes());
    println!("Unoptimized:\n{}", llvm_ir);

    //    llvm::optimise_ir(&mut llvm_module, 3);
    //    let llvm_ir_cstr = llvm_module.to_cstring();
    //    let llvm_ir = String::from_utf8_lossy(llvm_ir_cstr.as_bytes());
    //    println!("Optimized:\n{}", llvm_ir);

    // Compile the LLVM IR to a temporary object file.
    let object_file = try!(convert_io_error(NamedTempFile::new()));
    let obj_file_path = object_file.path().to_str().expect("path not valid utf-8");
    println!("Writing object file to {}", obj_file_path);
    llvm::write_object_file(&mut llvm_module, &obj_file_path).unwrap();

    let output_name = executable_name(path);
    println!("Outputting executable to {}", output_name);
    link_object_file(&obj_file_path, &output_name, target_triple).unwrap();

    // TODO: Enable executable stripping.
    //    fn strip_executable(executable_path: &str) -> Result<(), String> {
    //        let strip_args = ["-s", &executable_path[..]];
    //        shell::run_shell_command("strip", &strip_args[..])
    //    }
    //    let strip_opt = matches.opt_str("strip").unwrap_or_else(|| "yes".to_owned());
    //    if strip_opt == "yes" {
    //        try!(strip_executable(&output_name))
    //    }

    Ok(())
}

fn link_object_file(
    object_file_path: &str,
    executable_path: &str,
    target_triple: Option<String>,
) -> Result<(), String> {
    // Link the object file.
    let clang_args = if let Some(ref target_triple) = target_triple {
        vec![
            object_file_path,
            "-target",
            &target_triple,
            "-o",
            &executable_path[..],
        ]
    } else {
        vec![object_file_path, "-o", &executable_path[..]]
    };

    shell::run_shell_command("clang-7", &clang_args[..])
}

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let args: Vec<_> = env::args().collect();

    let mut opts = Options::new();

    opts.optflag("j", "jlang", "lex/parse jlang");
    opts.optflag("h", "help", "print usage");
    opts.optflag("v", "version", "print jcompiler version");
    opts.optflag("", "dump-llvm", "print LLVM IR generated");
    opts.optflag("", "dump-ir", "print BF IR generated");

    opts.optopt("O", "opt", "optimization level (0 to 2)", "LEVEL");
    opts.optopt("", "llvm-opt", "LLVM optimization level (0 to 3)", "LEVEL");
    opts.optopt(
        "",
        "passes",
        "limit jcompiler optimisations to those specified",
        "PASS-SPECIFICATION",
    );
    opts.optopt(
        "",
        "strip",
        "strip symbols from the binary (default: yes)",
        "yes|no",
    );

    let default_triple_cstring = llvm::get_default_target_triple();
    let default_triple = default_triple_cstring.to_str().unwrap();

    opts.optopt(
        "",
        "target",
        &format!("LLVM target triple (default: {})", default_triple),
        "TARGET",
    );

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(_) => {
            print_usage(&args[0], opts);
            std::process::exit(1);
        }
    };

    if matches.opt_present("h") {
        print_usage(&args[0], opts);
        return;
    }

    if matches.opt_present("v") {
        println!("jcompiler {}", VERSION);
        return;
    }

    if matches.free.len() != 1 {
        print_usage(&args[0], opts);
        std::process::exit(1);
    }

    if matches.opt_present("j") {
        match compile_jlang_file(&matches) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(2);
            }
        };
    }
}

#[test]
fn executable_name_test() {
    assert_eq!(executable_name("test.ijs"), "test");
}

#[test]
fn executable_name_relative_path_test() {
    assert_eq!(executable_name("dir/test.ijs"), "test");
}
