use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;

use itertools::free::join;
use parser::{AstNode, MonadicVerb, DyadicVerb};
use itertools::Itertools;
use itertools::EitherOrBoth::{Both, Left, Right};

type Offset = i32;

pub struct ARMBackend {}

#[derive(Debug)]
struct BasicBlock {
    frame_pointer: i32,
    frame_size: i32,
    instructions: Vec<ArmIns>,
}

#[derive(Debug)]
enum ArmIns {
    Load {
        dst: &'static str,
        src: &'static str,
    },
    Store {
        dst: &'static str,
        src: &'static str,
    },
    LoadOffset {
        dst: &'static str,
        src: &'static str,
        offsets: Vec<i32>,
    },
    StoreOffset {
        dst: &'static str,
        src: &'static str,
        offsets: Vec<i32>,
    },
    BranchAndLink {
        addr: &'static str,
    },
    Add {
        dst: &'static str,
        src: &'static str,
        add: &'static str,
    },
    AddImm {
        dst: &'static str,
        src: &'static str,
        imm: i32,
    },
    Sub {
        dst: &'static str,
        src: &'static str,
        sub: &'static str,
    },
    SubImm {
        dst: &'static str,
        src: &'static str,
        imm: i32,
    },
    Move {
        dst: &'static str,
        src: &'static str,
    },
    MoveImm {
        dst: &'static str,
        imm: i32,
    },
    Multiply {
        dst: &'static str,
        src: &'static str,
        mul: &'static str,
    },
    Compare {
        lhs: &'static str,
        rhs: &'static str,
    },
    MoveLT { dst: &'static str, src: &'static str },
    MoveLE { dst: &'static str, src: &'static str },
    MoveGT { dst: &'static str, src: &'static str },
    MoveGE { dst: &'static str, src: &'static str },
    MoveNE { dst: &'static str, src: &'static str },
    MoveEQ { dst: &'static str, src: &'static str },
    Nop
}

impl std::fmt::Display for ArmIns {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match self {
            ArmIns::Load { dst, src } => f.write_str(format!("ldr {}, {}", dst, src).as_str()),
            ArmIns::Store { dst, src } => f.write_str(format!("str {}, [{}]", src, dst).as_str()),
            ArmIns::LoadOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("ldr {}, [{}, {}]", dst, src, offset_str).as_str())
            }
            ArmIns::StoreOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("str {}, [{}, {}]", src, dst, offset_str).as_str())
            }
            ArmIns::Compare { lhs, rhs } => {
                f.write_str(format!("cmp {}, {}", lhs, rhs).as_str())
            }
            ArmIns::MoveLT { dst, src } => {
                f.write_str(format!("movlt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveLE { dst, src } => {
                f.write_str(format!("movle {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGT { dst, src } => {
                f.write_str(format!("movgt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGE { dst, src } => {
                f.write_str(format!("movge {}, {}", dst, src).as_str())
            }
            ArmIns::MoveEQ { dst, src } => {
                f.write_str(format!("moveq {}, {}", dst, src).as_str())
            }
            ArmIns::MoveNE { dst, src } => {
                f.write_str(format!("movne {}, {}", dst, src).as_str())
            }
            ArmIns::BranchAndLink { addr } => f.write_str(format!("bl {}", addr).as_str()),
            ArmIns::Move { dst, src } =>
                f.write_str(format!("mov {}, {}", dst, src).as_str()),
            ArmIns::MoveImm { dst, imm } => f.write_str(format!("mov {}, {}", dst, imm).as_str()),
            ArmIns::AddImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("add {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("add {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::SubImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("sub {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("sub {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::Sub { dst, src, sub } => {
                f.write_str(format!("sub {}, {}, {}", dst, src, sub).as_str())
            }
            ArmIns::Add { dst, src, add } => {
                f.write_str(format!("add {}, {}, {}", dst, src, add).as_str())
            }
            ArmIns::Multiply { dst, src, mul } => {
                f.write_str(format!("mul {}, {}, {}", dst, src, mul).as_str())
            }
            ArmIns::Nop => f.write_str("nop")
        }
    }
}

impl BasicBlock {
    fn new(frame_size: i32) -> BasicBlock {
        println!("Allocating new basic block with frame size {}", frame_size);
        BasicBlock {
            frame_size,
            frame_pointer: 0,
            instructions: vec![
                // Frame pointer starts out at stack pointer.
                ArmIns::Move {
                    dst: "fp",
                    src: "sp",
                },
                // Expand fp to size of frame.
                ArmIns::SubImm {
                    dst: "fp",
                    src: "fp",
                    imm: frame_size,
                },
                // Expand sp along with it.
                ArmIns::Move {
                    dst: "sp",
                    src: "fp"
                }
            ]
        }
    }
    fn stack_allocate(&mut self, width: i32) -> i32 {
        if self.frame_pointer + width > self.frame_size {
            panic!("Attempted to allocate entity of width {} which overflows frame size of {}; frame pointer is {}", width, self.frame_size, self.frame_pointer);
        }
        let offset = self.frame_pointer;
        self.frame_pointer += width;
        offset
    }
}

impl ::Backend for ARMBackend {
    fn compile_ast(
        &self,
        _path: &str,
        ast: &Vec<parser::AstNode>,
        _do_report_mem_usage: bool,
        _do_verbose: bool,
        output_path: String,
    ) -> Result<(), String> {
        for astnode in ast {
            println!("{:?}", astnode);
        }

        let assembly_filename = format!("{}.s", output_path);
        let assembly_file =
            File::create(&assembly_filename).expect("Intermediate file in which to write assembly");
        println!("Writing assembly file to {}", &assembly_filename);

        let mut basic_blocks = Vec::new();

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut basic_block = BasicBlock::new(compute_frame_size(expr));
                    let val_offsets = compile_expr(&mut basic_block, expr);

                    for (idx, offset) in val_offsets.iter().enumerate() {

                        basic_block.instructions.push(ArmIns::LoadOffset {
                            dst: "r1",
                            src: "fp",
                            offsets: vec![*offset],
                        });
                        basic_block.instructions.push(ArmIns::BranchAndLink {
                            addr: "jprint"
                        });

                        // Multiple printed terms are separated by space, except for the last item
                        if idx != val_offsets.len() - 1 {
                            basic_block.instructions.push(ArmIns::Load {
                                dst: "r0",
                                src: "=space_fmt",
                            });
                            basic_block
                                .instructions
                                .push(ArmIns::BranchAndLink { addr: "printf" });
                        }
                    }

                    // All printed expressions are terminated with a newline.
                    basic_block.instructions.push(ArmIns::Load {
                        dst: "r0",
                        src: "=nl_fmt",
                    });
                    basic_block
                        .instructions
                        .push(ArmIns::BranchAndLink { addr: "printf" });

                    // Erases the frame for this block.
                    basic_block.instructions.push(ArmIns::AddImm{
                        dst:"fp",
                        src:"fp",
                        imm: basic_block.frame_size,
                    });
                    // Stack pointer must be moved up as well.
                    basic_block.instructions.push(ArmIns::Move {
                        dst: "sp",
                        src: "fp"
                    });

                    basic_blocks.push(basic_block);
                }
                _ => panic!("Not ready to compile top-level AST node: {:?}", astnode),
            }
        }

        println!("Printing ARM basic blocks...");
        for basic_block in &basic_blocks {
            println!("{:?}", basic_block);
        }

        // TODO: Move boilerplate writing of preamble/postamble elsewhere.
        println!("Writing ARM...");
        let preamble = vec![
            ".arch armv7-a".to_string(),
            ".data".to_string(),
            "pos_int_fmt: .asciz \"%d\"".to_string(),
            "neg_int_fmt: .asciz \"_%d\"".to_string(),
            "nl_fmt:  .asciz \"\\n\"".to_string(),
            "space_fmt:  .asciz \" \"".to_string(),
            ".text".to_string(),
            ".global main".to_string(),
            ".extern printf".to_string(),
            ".syntax unified".to_string(),
            // printing related functions
            "jprint:".to_string(),
            "push {lr}".to_string(),
            "cmp r1, #0".to_string(),
            "blt jprint_neg".to_string(),
            "ldr r0, =pos_int_fmt".to_string(),
            "jprint_main:".to_string(),
            "bl printf".to_string(),
            "pop {lr}".to_string(),
            "bx lr".to_string(),
            "jprint_neg:".to_string(),
            "ldr r0, =neg_int_fmt".to_string(),
            "rsblt r1, r1, #0".to_string(), // takes abs value of r1
            "bl jprint_main".to_string(),
            // main
            "main:".to_string(),
            "push {ip, lr}".to_string(),
        ];
        for instr in preamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }
        for basic_block in &basic_blocks {
            for instr in &basic_block.instructions {
                writeln!(&assembly_file, "{}", instr).expect("write failure");
            }
        }
        let postamble = vec!["pop {ip, pc}".to_string()];
        for instr in postamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }

        let args = vec![
            "assemble-and-link-armv7.sh",
            &assembly_filename[..],
            &output_path,
        ];
        ::shell::run_shell_command("sh", &args)?;

        Ok(())
    }
}

fn compile_expr(basic_block: &mut BasicBlock, expr: &AstNode) -> Vec<Offset> {
    match expr {
        parser::AstNode::Integer(int) => {
            basic_block.instructions.push(ArmIns::MoveImm {
                dst: "r7",
                imm: *int,
            });
            let offset = basic_block.stack_allocate(4);
            basic_block.instructions.push(ArmIns::StoreOffset {
                dst: "fp",
                src: "r7",
                offsets: vec![offset],
            });
            vec![offset]
        }
        parser::AstNode::Terms(terms) => {
            let mut val_offsets = vec![];
            for term in terms {
                val_offsets.extend(compile_expr(basic_block, term));
            }
            val_offsets
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            basic_block.instructions.push(ArmIns::Nop);
            let val_offsets = compile_expr(basic_block, expr);
            basic_block.instructions.push(ArmIns::Nop);
            for offset in &val_offsets {
                basic_block.instructions.push(ArmIns::LoadOffset {
                    dst: "r4",
                    src: "fp",
                    offsets: vec![*offset]
                });
                match verb {
                    MonadicVerb::Increment => {
                        basic_block.instructions.push(ArmIns::AddImm {
                            dst: "r4",
                            src: "r4",
                            imm: 1
                        });
                    },
                    MonadicVerb::Square => {
                        basic_block.instructions.push(ArmIns::Multiply {
                            dst: "r4",
                            src: "r4",
                            mul: "r4"
                        });
                    },
                    MonadicVerb::Negate => {
                        basic_block.instructions.push(ArmIns::MoveImm {
                            dst: "r6",
                            imm: 0
                        });
                        basic_block.instructions.push(ArmIns::Sub {
                            dst: "r4",
                            src: "r6",
                            sub: "r4"
                        });
                    }
                    _ => unimplemented!("TODO: Support monadic verb: {:?}", verb)
                }
                basic_block.instructions.push(ArmIns::StoreOffset {
                    src: "r4",
                    dst: "fp",
                    offsets: vec![*offset]
                });
            }
            basic_block.instructions.push(ArmIns::Nop);
            val_offsets   // we return the same list of offsets,
                          // because we've updated them in-place
        },
        parser::AstNode::DyadicOp {verb, lhs, rhs} => {
            basic_block.instructions.push(ArmIns::Nop);
            let rhs_offsets = compile_expr(basic_block, rhs);
            let lhs_offsets = compile_expr(basic_block, lhs);
            basic_block.instructions.push(ArmIns::Nop);
            if rhs_offsets.len() != lhs_offsets.len()
                && (lhs_offsets.len() != 1 && rhs_offsets.len() != 1) {
                    panic!("Dyadic op lhs has length {}, rhs has length {}; don't know how to proceed.", lhs_offsets.len(), rhs_offsets.len())
            }

            #[derive(Debug, Eq, PartialEq)]
            enum Destination { LHS, RHS }
            let dest = if rhs_offsets.len() > lhs_offsets.len() { Destination::RHS } else { Destination::LHS };
            let repeated_offset = match lhs_offsets.len() {
                1 => lhs_offsets.get(0).unwrap(),
                _ => rhs_offsets.get(0).unwrap()
            };

            for pair in lhs_offsets.iter().zip_longest(rhs_offsets.iter()) {
                let (l, r) = match pair {
                    Both(l, r) => (l, r),
                    Left(l) => (l, repeated_offset),
                    Right(r) => (repeated_offset, r)
                };
                basic_block.instructions.push(
                    ArmIns::LoadOffset { dst: "r3", src: "fp", offsets: vec![*l] });
                basic_block.instructions.push(
                    ArmIns::LoadOffset { dst: "r4", src: "fp", offsets: vec![*r] });
                match verb {
                    DyadicVerb::Plus =>
                        basic_block.instructions.push(
                            ArmIns::Add { dst: "r4", src: "r3", add: "r4" }),
                    DyadicVerb::Times =>
                        basic_block.instructions.push(
                            ArmIns::Multiply { dst: "r4", src: "r3", mul: "r4" }),
                    DyadicVerb::Minus =>
                        basic_block.instructions.push(
                            ArmIns::Sub { dst: "r4", src: "r3", sub: "r4" }),
                    DyadicVerb::LessThan => {
                        basic_block.instructions.push(
                            ArmIns::Compare { lhs: "r3", rhs: "r4" });
                        basic_block.instructions.push(
                            ArmIns::MoveLT { dst: "r4", src: "#1" });
                        basic_block.instructions.push(
                            ArmIns::MoveGE { dst: "r4", src: "#0" });
                    },
                    DyadicVerb::Equal => {
                        basic_block.instructions.push(
                            ArmIns::Compare { lhs: "r3", rhs: "r4" });
                        basic_block.instructions.push(
                            ArmIns::MoveEQ { dst: "r4", src: "#1" });
                        basic_block.instructions.push(
                            ArmIns::MoveNE { dst: "r4", src: "#0" });
                    }
                    DyadicVerb::LargerThan => {
                        basic_block.instructions.push(
                            ArmIns::Compare { lhs: "r3", rhs: "r4" });
                        basic_block.instructions.push(
                            ArmIns::MoveGT { dst: "r4", src: "#1" });
                        basic_block.instructions.push(
                            ArmIns::MoveLE { dst: "r4", src: "#0" });
                    }
                    _ => panic!("Not ready to compile dyadic verb: {:?}", verb)
                }
                basic_block.instructions.push(ArmIns::StoreOffset {
                    src: "r4",
                    dst: "fp",
                    offsets: vec![if dest == Destination::RHS { *r } else { *l }]
                });
            }
            // return whichever set of offsets we actually stored to
            if dest == Destination::RHS { rhs_offsets } else { lhs_offsets }
        },
        parser::AstNode::Reduce {verb, expr} => {
            basic_block.instructions.push(ArmIns::Nop);
            let expr_offsets = compile_expr(basic_block, expr);
            basic_block.instructions.push(ArmIns::Nop);
            // Initialize the accumulator to expr's last offset value
            let accum_offset = *expr_offsets.last().unwrap();
            basic_block.instructions.push(ArmIns::LoadOffset {
                dst: "r3",
                src: "fp",
                offsets: vec![accum_offset]
            });
            // Accumulate from right to left.
            for offset_idx in expr_offsets[0..expr_offsets.len()-1].iter().rev() {
                basic_block.instructions.push(ArmIns::LoadOffset {
                    dst: "r4",
                    src: "fp",
                    offsets: vec![*offset_idx]
                });
                match verb {
                    DyadicVerb::Plus => {
                        basic_block.instructions.push(ArmIns::Add {
                            dst: "r3",
                            src: "r4",
                            add: "r3"
                        });
                    },
                    DyadicVerb::Minus => {
                        basic_block.instructions.push(ArmIns::Sub {
                            dst: "r3",
                            src: "r4",
                            sub: "r3"
                        });
                    },
                    DyadicVerb::Times => {
                        basic_block.instructions.push(ArmIns::Multiply {
                            dst: "r3",
                            src: "r4",
                            mul: "r3"
                        });
                    },
                    _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb)
                }
            }
            // Store the accumulator in expr's first offset, and return that
            // single offset here.
            basic_block.instructions.push(ArmIns::StoreOffset {
                src: "r3",
                dst: "fp",
                offsets: vec![accum_offset]
            });
            vec![accum_offset]
        },
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}

fn compute_frame_size(expr: &AstNode) -> i32 {
    match expr {
        parser::AstNode::Integer(_int) => 4,
        parser::AstNode::Terms(terms) =>
            terms.iter().map(|e| compute_frame_size(e)).sum(),
        parser::AstNode::MonadicOp {verb: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::DyadicOp {verb: _, lhs, rhs} =>
            compute_frame_size(lhs) + compute_frame_size(rhs),
        parser::AstNode::Reduce {verb: _, expr} =>
            compute_frame_size(expr),
        _ => panic!("Not ready to compute frame size of expr: {:?}", expr)
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<dyn (::Backend)>, String> {
    Ok(Box::new(ARMBackend {}))
}
