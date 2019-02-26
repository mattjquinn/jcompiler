#![warn(trivial_numeric_casts)]

//! Compile time execution of BF programs.

use std::num::Wrapping;

use bfir::{AstNode, Cell};

use bounds::highest_cell_index;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionState<'a> {
    pub start_instr: Option<&'a AstNode>,
    pub cells: Vec<Cell>,
    pub cell_ptr: isize,
    pub outputs: Vec<i8>,
}

impl<'a> ExecutionState<'a> {
    pub fn initial(instrs: &[AstNode]) -> Self {
        ExecutionState {
            start_instr: None,
            cells: vec![Wrapping(0); highest_cell_index(instrs) + 1],
            cell_ptr: 0,
            outputs: vec![],
        }
    }
}
