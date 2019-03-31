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
use jcompilerlib::compiler;
use std::env;

fn main() {
    let args: Vec<_> = env::args().collect();

    let mut opts = Options::new();

    opts.optflag("h", "help", "print usage");
    opts.optflag("v", "version", "print jcompiler version");
    opts.optflag("", "verbose", "print AST, IR, etc.");
    opts.optflag("m", "mem-usage", "binary will print memory usage");
    opts.optopt("", "llvm-opt", "LLVM optimization level (0 to 3)", "LVL");
    opts.optopt("", "strip", "strip symbols from the binary (default: yes)", "yes|no");

    let default_triple_cstring = compiler::get_default_target_triple();
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

    let llvm_opt_level : u8 = match matches.opt_str("llvm-opt") {
        Some(lvlstr) =>
            match lvlstr.parse::<u8>() {
                Ok(n) if n <= 3 => n,
                _ => {
                    println!("Unrecognized choice \"{}\" for --llvm-opt; need \"0\", \"1\", \"2\", or \"3\".", lvlstr);
                    return;
                }
        }
        _ => 0,
    };

    let do_strip = match matches.opt_str("strip") {
        Some(ans) => {
            match &ans[..] {
                "yes" => true,
                "no" => false,
                _ => {
                    println!("Unrecognized choice \"{}\" for --strip; need \"yes\" or \"no\".", ans);
                    return;
                }
            }
        },
        _ => true // Strip executables of debugging symbols by default.
    };

    match jcompilerlib::compile(&matches.free[0],
                                matches.opt_str("target"),
                                llvm_opt_level,
                                do_strip,
                                matches.opt_present("mem-usage"),
                                matches.opt_present("verbose"),
                                None) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(2);
        }
    };
}