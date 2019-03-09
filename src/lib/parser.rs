use self::AstNode::*;
use itertools::Itertools;
use pest::error::Error;
use std::fmt;

use pest::Parser;

#[derive(Parser)]
#[grammar = "j.pest"]
pub struct JParser;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Number(u32),
    BinAdd {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    BinMul {
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    Terms(Vec<AstNode>),
    Increment(Vec<AstNode>),
    Square(Vec<AstNode>),
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "{:?}", self);
        Ok(())
    }
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = JParser::parse(Rule::program, source)?;
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        //        println!("Rule:    {:?}", pair.as_rule());
        //        println!("Span:    {:?}", pair.as_span());
        //        println!("Text:    {}", pair.as_str());

        match pair.as_rule() {
            Rule::expr => {
                ast.push(Print(Box::new(build_ast_from_expr(pair))));
            }
            _ => {}
        }
    }

    Ok(ast)
}

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // IMPORTANT: Binary operations are right-associative in J, i.e.:
    // 3 * 2 + 1  is evaluated as 3 * (2 + 1)

    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::monadicExpr => {
            let mut pair = pair.into_inner();
            let verb = pair.next().unwrap().as_str();
            let expr = pair.next().unwrap();
            match verb {
                ">:" => Increment(vec![build_ast_from_expr(expr)]),
                "*:" => Square(vec![build_ast_from_expr(expr)]),
                _ => panic!("Unsupported monadic verb in expr: {}", verb),
            }
        }
        dexpr @ Rule::dyadicExpr => panic!("TODO: Build dyadic expr:    {:?}", dexpr),
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
        Rule::terms => Terms(build_ast_from_terms(pair)),
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}
fn build_ast_from_terms(pair: pest::iterators::Pair<Rule>) -> Vec<AstNode> {
    pair.into_inner()
        .map(build_ast_from_term)
        .collect_vec()
}

fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::number => AstNode::Number(pair.as_str().trim().parse().unwrap()),
        Rule::expr => build_ast_from_expr(pair),
        unknown_term => panic!("Unexpected term: {:?}", unknown_term),
    }
}
