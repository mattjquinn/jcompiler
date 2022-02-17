extern crate getopts;
/// Integration tests for just the parser.
extern crate jcompilerlib;

use getopts::Options;

use jcompilerlib::backend;
use jcompilerlib::parser;

#[test]
fn parse_test1() {
    let src = "8";
    let ast = parser::parse(src);
    assert!(ast.is_ok());
    assert_eq!("[Print(Integer(8))]", format!("{:?}", ast.unwrap()));
}

#[test]
fn test() {
    let mut opts = Options::new();
    backend::register_cli_options(&mut opts);
    assert!(!opts.usage("").is_empty())
}
