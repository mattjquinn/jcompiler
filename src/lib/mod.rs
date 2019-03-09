#![warn(trivial_numeric_casts)]

extern crate ansi_term;
extern crate getopts;
extern crate itertools;
extern crate llvm_sys;
extern crate matches;
extern crate pest;
extern crate quickcheck;
extern crate rand;
extern crate regex;
extern crate tempfile;

#[macro_use]
extern crate pest_derive;

pub mod llvm;
pub mod parser;
pub mod shell;

use getopts::Options;
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

pub fn print_usage(bin_name: &str, opts: Options) {
    let brief = format!("Usage: {} SOURCE_FILE [options]", bin_name);
    print!("{}", opts.usage(&brief));
}

fn convert_io_error<T>(result: Result<T, std::io::Error>) -> Result<T, String> {
    match result {
        Ok(value) => Ok(value),
        Err(e) => Err(format!("{}", e)),
    }
}

pub fn compile(path: &str,
               target_triple : Option<String>,
               output_path : Option<String>) -> Result<(), String> {
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

    let output_path = match output_path {
        Some(op) => op,
        None => executable_name(path),
    };
    println!("Outputting executable to {}", output_path);
    link_object_file(&obj_file_path, &output_path, target_triple).unwrap();

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

#[test]
fn executable_name_test() {
    assert_eq!(executable_name("test.ijs"), "test");
}

#[test]
fn executable_name_relative_path_test() {
    assert_eq!(executable_name("dir/test.ijs"), "test");
}
