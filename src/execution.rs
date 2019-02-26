#![warn(trivial_numeric_casts)]

//! Compile time execution of BF programs.

use std::num::Wrapping;

use bfir::{AstNode, Cell};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionState<'a> {
    pub start_instr: Option<&'a AstNode>,
    pub cells: Vec<Cell>,
    pub cell_ptr: isize,
    pub outputs: Vec<i8>,
}

impl<'a> ExecutionState<'a> {
    pub fn initial() -> Self {
        ExecutionState {
            start_instr: None,
            cells: vec![Wrapping(0); 99999],
            cell_ptr: 0,
            outputs: vec![],
        }
    }
}
