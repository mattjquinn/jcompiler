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

use getopts::Options;
use jcompilerlib::backend;
use std::env;

fn main() {
    let args: Vec<_> = env::args().collect();

    let mut opts = Options::new();

    // Allow all backends to register their CLI options.
    backend::register_cli_options(&mut opts);

    // Register CLI options common to all backends.
    opts.optflag("m", "mem-usage", "binary will print memory usage");
    opts.optflag("v", "version", "print jcompiler version");
    opts.optflag("V", "verbose", "print AST, IR, etc.");
    opts.optflag("h", "help", "print usage");

    // Parse options; print usage and exit if there's a problem.
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(_) => {
            print_usage(&args[0], opts);
            std::process::exit(1);
        }
    };

    // Display help information if so requested.
    if matches.opt_present("h") {
        print_usage(&args[0], opts);
        return;
    }

    // Display version information if so requested.
    if matches.opt_present("v") {
        println!("jcompiler {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    // We expect a single unbound argument: the J file to be compiled.
    if matches.free.len() != 1 {
        print_usage(&args[0], opts);
        std::process::exit(1);
    }

    // Get a Backend struct.
    let backend = match backend::init_from_cli_options(&matches) {
        Ok(backend) => backend,
        Err(err) =>{
            eprintln!("{}", err);
            return;
        }
    };

    match jcompilerlib::compile(
        &matches.free[0],
        backend,
        matches.opt_present("mem-usage"),
        matches.opt_present("verbose"),
        None
    ) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(2);
        }
    };
}

pub fn print_usage(bin_name: &str, opts: Options) {
    let brief = format!("Usage: {} SOURCE_FILE [options]", bin_name);
    print!("{}", opts.usage(&brief));
}

