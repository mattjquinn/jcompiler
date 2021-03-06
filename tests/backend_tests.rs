/// Integration tests for just the parser.
extern crate jcompilerlib;
extern crate getopts;

use getopts::Options;

use jcompilerlib::backend;

#[test]
fn test() {
    let mut opts = Options::new();
    backend::register_cli_options(&mut opts);
    assert!(opts.usage("").len() > 0);
}
