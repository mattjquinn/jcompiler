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
    BinAdd {lhs: Box<AstNode>, rhs: Box<AstNode>},
    BinMul {lhs: Box<AstNode>, rhs: Box<AstNode>},
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "{:?}", self);
        Ok(())
    }
}

pub fn parse(source : &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let mut pairs = JParser::parse(Rule::program, source)?;
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
//        println!("Rule:    {:?}", pair.as_rule());
//        println!("Span:    {:?}", pair.as_span());
//        println!("Text:    {}", pair.as_str());

        match pair.as_rule() {
            Rule::expr => {
                ast.push(Print(Box::new(
                    build_ast_from_expr(pair.into_inner()))));
//                let num = pair.into_inner().next().unwrap().as_str();
//                ast.push(Print(Box::new(Number(num.parse().unwrap()))));
            },
//            Rule::exprBinOp => {
//                let mut inner_terms = pair.into_inner();
//                let lhs : u32 = inner_terms.next().unwrap().as_str().trim()
//                    .parse().unwrap();
//                let op = inner_terms.next().unwrap().as_str().trim();
//                let rhs : u32 = inner_terms.next().unwrap().as_str().trim()
//                    .parse().unwrap();
//                match op {
//                    "+" => ast.push(Print(Box::new(BinAdd{lhs, rhs}))),
//                    "*" => ast.push(Print(Box::new(BinMul{lhs, rhs}))),
//                    _ => panic!("Parsed unexpected binary op: {}", op)
//                }
//            },
            rule => {}
        }
    }

    Ok(ast)
}

fn build_ast_from_expr(mut pairs : pest::iterators::Pairs<Rule>) -> AstNode {
    // IMPORTANT: Binary operations are right-associative in J, i.e.:
    // 3 * 2 + 1  is evaluated as 3 * (2 + 1)

    // TODO: If LHS is expr, recurse. If LHS is number, create Number node.
    // TODO: If RHS is present, recurse. Join together under AstNode.
    // TODO: Change AstNode to Box<AstNode> for binary operations.

    let lhspair = pairs.next().unwrap();
    let lhs = match lhspair.as_rule() {
        Rule::number => AstNode::Number(lhspair.as_str().trim().parse().unwrap()),
        Rule::expr => build_ast_from_expr(lhspair.into_inner()),
        _ => panic!("Unexpected LHS inside expr: {}", lhspair)
    };

    if let Some(op) = pairs.next() {
        let opstr = match op.as_rule() {
            Rule::dyadicVerb => op.as_str().trim(),
            _ => panic!("Expected dyadic verb inside expr, got: {}", op.as_str())
        };
        let rhspair = pairs.next().unwrap();
        let rhs = match rhspair.as_rule() {
            Rule::number => AstNode::Number(rhspair.as_str().trim().parse().unwrap()),
            Rule::expr => build_ast_from_expr(rhspair.into_inner()),
            _ => panic!("Unexpected RHS inside expr: {}", rhspair)
        };
        match opstr {
            "+" => AstNode::BinAdd{lhs: Box::new(lhs), rhs: Box::new(rhs)},
            "*" => AstNode::BinMul{lhs: Box::new(lhs), rhs: Box::new(rhs)},
            _ => panic!("Unexpected binary op: {}", opstr)
        }
    } else {
        lhs
    }
}
