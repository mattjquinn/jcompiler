#![warn(trivial_numeric_casts)]

//! Calculate the maximum cell accessed by a BF program.

#[cfg(test)]
use std::num::Wrapping;

use std::cmp::{max, Ord, Ordering};
use std::ops::Add;

use bfir::AstNode;
use bfir::AstNode::*;

#[cfg(test)]
use bfir::{parse, Position};

// 100,000 cells, zero-indexed.
pub const MAX_CELL_INDEX: usize = 99999;

/// Return the highest cell index that can be reached during program
/// execution. Zero-indexed.
pub fn highest_cell_index(instrs: &[AstNode]) -> usize {
    let (highest_index, _) = overall_movement(instrs);

    match highest_index {
        SaturatingInt::Number(x) => {
            if x > MAX_CELL_INDEX as i64 {
                // TODO: generate a warning here.
                MAX_CELL_INDEX
            } else {
                x as usize
            }
        }
        SaturatingInt::Max => MAX_CELL_INDEX,
    }
}

/// Saturating arithmetic: we have normal integers that work as
/// expected, but Max is bigger than any Number.
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
enum SaturatingInt {
    Number(i64),
    Max,
}

impl Add for SaturatingInt {
    type Output = SaturatingInt;
    fn add(self, rhs: SaturatingInt) -> SaturatingInt {
        if let (&SaturatingInt::Number(x), &SaturatingInt::Number(y)) = (&self, &rhs) {
            SaturatingInt::Number(x + y)
        } else {
            SaturatingInt::Max
        }
    }
}

impl Ord for SaturatingInt {
    fn cmp(&self, other: &SaturatingInt) -> Ordering {
        match (self, other) {
            (&SaturatingInt::Max, &SaturatingInt::Max) => Ordering::Equal,
            (&SaturatingInt::Number(_), &SaturatingInt::Max) => Ordering::Less,
            (&SaturatingInt::Max, &SaturatingInt::Number(_)) => Ordering::Greater,
            (&SaturatingInt::Number(x), &SaturatingInt::Number(y)) => x.cmp(&y),
        }
    }
}

impl PartialOrd for SaturatingInt {
    fn partial_cmp(&self, other: &SaturatingInt) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Return a tuple (highest cell index reached, cell index at end).
/// If movement is unbounded, return Max.
fn overall_movement(instrs: &[AstNode]) -> (SaturatingInt, SaturatingInt) {
    let mut net_movement = SaturatingInt::Number(0);
    let mut max_index = SaturatingInt::Number(0);

    for (instr_highest_offset, instr_net_movement) in instrs.iter().map(movement) {
        max_index = max(
            net_movement,
            max(net_movement + instr_highest_offset, max_index),
        );
        net_movement = net_movement + instr_net_movement;
    }
    (max_index, net_movement)
}

/// Return a tuple (highest cell index reached, cell index at end).
/// If movement is unbounded, return Max.
fn movement(instr: &AstNode) -> (SaturatingInt, SaturatingInt) {
    match *instr {
        PointerIncrement { amount, .. } => {
            if amount < 0 {
                (
                    SaturatingInt::Number(0),
                    SaturatingInt::Number(amount as i64),
                )
            } else {
                (
                    SaturatingInt::Number(amount as i64),
                    SaturatingInt::Number(amount as i64),
                )
            }
        }
        Increment { offset, .. } => (
            SaturatingInt::Number(offset as i64),
            SaturatingInt::Number(0),
        ),
        Loop { ref body, .. } => {
            let (max_in_body, net_in_body) = overall_movement(body);

            match net_in_body {
                SaturatingInt::Number(net_loop_movement) => {
                    if net_loop_movement == 0 {
                        (max_in_body, SaturatingInt::Number(0))
                    } else if net_loop_movement < 0 {
                        // Net movement was negative, so conservatively assume
                        // it was zero (e.g. the loop may run zero times).
                        (max_in_body, SaturatingInt::Number(0))
                    } else {
                        // Net loop movement was positive, so we can't
                        // assume any bounds.
                        (SaturatingInt::Max, SaturatingInt::Max)
                    }
                }
                SaturatingInt::Max => {
                    // Unbounded movement somewhere inside the loop,
                    // so this loop is unbounded.
                    (SaturatingInt::Max, SaturatingInt::Max)
                }
            }
        }
        Read { .. } | Write { .. } => (SaturatingInt::Number(0), SaturatingInt::Number(0)),
    }
}

#[test]
fn one_cell_bounds() {
    let instrs = parse("+-.,").unwrap();
    assert_eq!(highest_cell_index(&instrs), 0);
}

#[test]
fn ptr_increment_bounds() {
    let instrs = parse(">").unwrap();
    assert_eq!(highest_cell_index(&instrs), 1);
}

#[test]
fn ptr_increment_sequence_bounds() {
    let instrs = parse(">>.<").unwrap();
    assert_eq!(highest_cell_index(&instrs), 2);

    let instrs = parse(">><>>").unwrap();
    assert_eq!(highest_cell_index(&instrs), 3);
}

#[test]
fn multiple_ptr_increment_bounds() {
    let instrs = vec![PointerIncrement {
        amount: 2,
        position: Some(Position { start: 0, end: 0 }),
    }];
    assert_eq!(highest_cell_index(&instrs), 2);
}

#[test]
fn unbounded_movement() {
    let instrs = parse("[>]").unwrap();
    assert_eq!(highest_cell_index(&instrs), MAX_CELL_INDEX);

    let instrs = parse(">[<]").unwrap();
    assert_eq!(highest_cell_index(&instrs), 1);
}

#[test]
fn excessive_bounds_truncated() {
    // TODO: we should generate a warning in this situation.
    let instrs = vec![PointerIncrement {
        amount: MAX_CELL_INDEX as isize + 1,
        position: Some(Position { start: 0, end: 0 }),
    }];
    assert_eq!(highest_cell_index(&instrs), MAX_CELL_INDEX);
}

#[test]
fn loop_with_no_net_movement() {
    // Max cell index 1, final cell position 0.
    let instrs = parse("[->+<]").unwrap();
    assert_eq!(highest_cell_index(&instrs), 1);

    // Max cell index 1, final cell position 1.
    let instrs = parse("[->+<]>").unwrap();
    assert_eq!(highest_cell_index(&instrs), 1);

    // Max cell index 2, final cell position 2.
    let instrs = parse("[->+<]>>").unwrap();
    assert_eq!(highest_cell_index(&instrs), 2);
}

#[test]
fn increment_offset_bounds() {
    let instrs = [Increment {
        amount: Wrapping(2),
        offset: 5,
        position: Some(Position { start: 0, end: 0 }),
    }];
    assert_eq!(highest_cell_index(&instrs), 5);
}
