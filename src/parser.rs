//! bfir defines an AST for BF. This datastructure represents the
//! original BF source code with position data so we can find the
//! source lines from a portion of AST.
//!
//! It also provides functions for generating ASTs from source code,
//! producing good error messages on malformed inputs.

use std::fmt;
use std::num::Wrapping;
use lexer::Token;
use self::AstNode::*;
use diagnostics::Position;

/// A cell is the fundamental BF datatype that we work with. BF
/// requires this to be at least one byte, we provide a cell of
/// exactly one byte.
pub type Cell = Wrapping<i8>;

/// `AstNode` represents a node in our BF AST.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstNode {
    Increment {
        amount: Cell,
        offset: isize,
        position: Option<Position>,
    },
    PointerIncrement {
        amount: isize,
        position: Option<Position>,
    },
    Read {
        position: Option<Position>,
    },
    Write {
        position: Option<Position>,
    },
    Loop {
        body: Vec<AstNode>,
        position: Option<Position>,
    },
    PrintExpressionStmt {
        expr: Box<AstNode>,
        position: Position,
    },
}

fn fmt_with_indent(instr: &AstNode, indent: i32, f: &mut fmt::Formatter) {
    for _ in 0..indent {
        let _ = write!(f, "  ");
    }

    match instr {
        &Loop {
            body: ref loop_body,
            position,
            ..
        } => {
            let _ = write!(f, "Loop position: {:?}", position);

            for loop_instr in loop_body {
                let _ = writeln!(f);
                fmt_with_indent(loop_instr, indent + 1, f);
            }
        }
        instr => {
            let _ = write!(f, "{:?}", instr);
        }
    }
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt_with_indent(self, 0, f);
        Ok(())
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub position: Position,
}

/// Given a string of BF source code, parse and return our BF IR
/// representation. If parsing fails, return an Info describing what
/// went wrong.
pub fn parse(tokens : &Vec<Token>) -> Result<Vec<AstNode>, ParseError> {
    // AstNodes in the current loop (or toplevel).
    let mut instructions = vec![];
    // Contains the instructions of open parent loops (or toplevel),
    // and the starting indices of the loops.
    //let mut stack = vec![];

    let mut tokstream = &tokens[..];

    // TODO: Parse each sequence of tokens up until the next newline as a
    // PrintExpressionStmt, where an expression is either a single number or
    // a binary operation. Regardless, the expression needs to be printed by the
    // resulting compiled program.

    //for () {
    //    match c {
    //        '+' => instructions.push(Increment {
    //            amount: Wrapping(1),
    //            offset: 0,
    //            position: Some(Position {
    //                start: index,
    //                end: index,
    //            }),
    //        }),
    //        '-' => instructions.push(Increment {
    //            amount: Wrapping(-1),
    //            offset: 0,
    //            position: Some(Position {
    //                start: index,
    //                end: index,
    //            }),
    //        }),
    //        '>' => instructions.push(PointerIncrement {
    //            amount: 1,
    //            position: Some(Position {
    //                start: index,
    //                end: index,
    //            }),
    //        }),
    //        '<' => instructions.push(PointerIncrement {
    //            amount: -1,
    //            position: Some(Position {
    //                start: index,
    //                end: index,
    //            }),
    //        }),
    //        ',' => instructions.push(Read {
    //            position: Some(Position {
    //                start: index,
    //                end: index,
    //            }),
    //        }),
    //        '.' => instructions.push(Write {
    //            position: Some(Position {
    //                start: index,
    //                end: index,
    //            }),
    //        }),
    //        '[' => {
    //            stack.push((instructions, index));
    //            instructions = vec![];
    //        }
    //        ']' => {
    //            if let Some((mut parent_instr, open_index)) = stack.pop() {
    //                parent_instr.push(Loop {
    //                    body: instructions,
    //                    position: Some(Position {
    //                        start: open_index,
    //                        end: index,
    //                    }),
    //                });
    //                instructions = parent_instr;
    //            } else {
    //                return Err(ParseError {
    //                    message: "This ] has no matching [".to_owned(),
    //                    position: Position {
    //                        start: index,
    //                        end: index,
    //                    },
    //                });
    //            }
    //        }
    //        _ => (),
    //    }
    //}

    //if !stack.is_empty() {
    //    let pos = stack.last().unwrap().1;
    //    return Err(ParseError {
    //        message: "This [ has no matching ]".to_owned(),
    //        position: Position {
    //            start: pos,
    //            end: pos,
    //        },
    //    });
    //}

    Ok(instructions)
}
