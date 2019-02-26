use std::fmt;
use self::AstNode::*;
use pest::error::Error;

use pest::Parser;

#[derive(Parser)]
#[grammar = "j.pest"]
pub struct JParser;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Number(u32),
    BinAdd {lhs: u32, rhs: u32},
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "{:?}", self);
        Ok(())
    }
}

pub fn parse(source : &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = JParser::parse(Rule::program, source)?;
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());

        match pair.as_rule() {
            Rule::exprNum => {
                let num = pair.into_inner().next().unwrap().as_str();
                ast.push(Print(Box::new(Number(num.parse().unwrap()))));
            },
            Rule::exprBinAdd => {
                let mut inner_terms = pair.into_inner();
                let lhs : u32 = inner_terms.next().unwrap().as_str().trim()
                    .parse().unwrap();
                let rhs : u32 = inner_terms.next().unwrap().as_str().trim()
                    .parse().unwrap();
                ast.push(Print(Box::new(BinAdd{lhs, rhs})));
            },
            _ => {},
        }
    }

    Ok(ast)
}
