extern crate jcompilerlib;

use jcompilerlib::parser;

#[test]
fn parse_test1() {
    let src = "8";
    let ast = parser::parse(src);
    assert!(ast.is_ok());
    assert_eq!("[Print(Terms([Number(8)]))]", format!("{:?}", ast.unwrap()));
}