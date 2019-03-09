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

use getopts::{Options};
use jcompilerlib::llvm;
use std::env;

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
            jcompilerlib::print_usage(&args[0], opts);
            std::process::exit(1);
        }
    };

    if matches.opt_present("h") {
        jcompilerlib::print_usage(&args[0], opts);
        return;
    }

    if matches.opt_present("v") {
        println!("jcompiler {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    if matches.free.len() != 1 {
        jcompilerlib::print_usage(&args[0], opts);
        std::process::exit(1);
    }

    match jcompilerlib::compile(&matches.free[0],
                                matches.opt_str("target"),
                                None) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(2);
        }
    };
}