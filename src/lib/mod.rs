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
