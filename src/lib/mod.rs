#![warn(trivial_numeric_casts)]

extern crate ansi_term;
extern crate ascii;
extern crate getopts;
extern crate itertools;
extern crate llvm_sys;
extern crate matches;
extern crate pest;
extern crate quickcheck;
extern crate rand;
extern crate regex;
extern crate tempfile;
extern crate ieee754;
extern crate linked_hash_set;

#[macro_use]
extern crate pest_derive;
extern crate core;

pub mod backend;
pub mod parser;
pub mod shell;

use backend::Backend;
use std::fs;
use std::path::Path;

pub fn compile(
    path: &str,
    backend: Box<dyn Backend>,
    do_report_mem_usage: bool,
    do_verbose: bool,
    optional_output_path: Option<String>,
) -> Result<(), String> {
    let jsrc = fs::read_to_string(path).expect("cannot open source of provided J program");

    let ast = match parser::parse(&jsrc[..]) {
        Ok(instrs) => instrs,
        Err(parse_error) => {
            panic!("{}", parse_error);
        }
    };

    if do_verbose {
        for astnode in &ast {
            println!("{:?}", astnode);
        }
    }

    let output_path = match optional_output_path {
        Some(p) => p,
        None => executable_name(path),
    };

    backend.compile_ast(path, &ast, do_report_mem_usage, do_verbose, output_path)
}

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

#[test]
fn executable_name_test() {
    assert_eq!(executable_name("test.ijs"), "test");
}

#[test]
fn executable_name_relative_path_test() {
    assert_eq!(executable_name("dir/test.ijs"), "test");
}
