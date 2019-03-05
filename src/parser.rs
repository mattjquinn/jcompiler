use std::fmt;
use self::AstNode::*;
use pest::error::Error;
use itertools::Itertools;

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
    Terms(Vec<AstNode>),
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
                    build_ast_from_expr(pair.into_inner().next().unwrap()))));
            },
            rule => {}
        }
    }

    Ok(ast)
}

fn build_ast_from_expr(mut pair : pest::iterators::Pair<Rule>) -> AstNode {
    // IMPORTANT: Binary operations are right-associative in J, i.e.:
    // 3 * 2 + 1  is evaluated as 3 * (2 + 1)

    match pair.as_rule() {
        mexpr @ Rule::monadicExpr =>
           panic!("TODO: Build monadic expr:    {:?}", mexpr),
        dexpr @ Rule::dyadicExpr =>
            panic!("TODO: Build dyadic expr:    {:?}", dexpr),
            // OLD CODE:
            //    let lhspair = pairs.next().unwrap();
            //    let lhs = match lhspair.as_rule() {
            //        Rule::number => AstNode::Number(lhspair.as_str().trim().parse().unwrap()),
            //        Rule::expr => build_ast_from_expr(lhspair.into_inner()),
            //        _ => panic!("Unexpected LHS inside expr: {}", lhspair)
            //    };
            //
            //    if let Some(op) = pairs.next() {
            //        let opstr = match op.as_rule() {
            //            Rule::dyadicVerb => op.as_str().trim(),
            //            _ => panic!("Expected dyadic verb inside expr, got: {}", op.as_str())
            //        };
            //        let rhspair = pairs.next().unwrap();
            //        let rhs = match rhspair.as_rule() {
            //            Rule::number => AstNode::Number(rhspair.as_str().trim().parse().unwrap()),
            //            Rule::expr => build_ast_from_expr(rhspair.into_inner()),
            //            _ => panic!("Unexpected RHS inside expr: {}", rhspair)
            //        };
            //        match opstr {
            //            "+" => AstNode::BinAdd{lhs: Box::new(lhs), rhs: Box::new(rhs)},
            //            "*" => AstNode::BinMul{lhs: Box::new(lhs), rhs: Box::new(rhs)},
            //            _ => panic!("Unexpected binary op: {}", opstr)
            //        }
            //    } else {
            //        lhs
            //    }
        Rule::terms =>
            Terms(pair.into_inner()
                .map(|p| build_ast_from_term(p)).collect_vec()),
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr)
    }
}

fn build_ast_from_term(mut pair : pest::iterators::Pair<Rule>) -> AstNode {

    match pair.as_rule() {
        Rule::number =>
            AstNode::Number(pair.as_str().trim().parse().unwrap()),
        Rule::expr =>
            build_ast_from_expr(pair),
        unknown_term => panic!("Unexpected term: {:?}", unknown_term)
    }
}
