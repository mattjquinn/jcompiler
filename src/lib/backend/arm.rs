use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;

use itertools::free::join;
use parser::{AstNode, MonadicVerb, DyadicVerb};

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
                        basic_block.instructions.push(ArmIns::Load {
                            dst: "r0",
                            src: "=intfmt",
                        });
                        basic_block.instructions.push(ArmIns::LoadOffset {
                            dst: "r1",
                            src: "fp",
                            offsets: vec![*offset],
                        });
                        basic_block
                            .instructions
                            .push(ArmIns::BranchAndLink { addr: "printf" });

                        // Multiple printed terms are separated by space, except for the last item
                        if idx != val_offsets.len() - 1 {
                            basic_block.instructions.push(ArmIns::Load {
                                dst: "r0",
                                src: "=spacefmt",
                            });
                            basic_block
                                .instructions
                                .push(ArmIns::BranchAndLink { addr: "printf" });
                        }
                    }

                    // All printed expressions are terminated with a newline.
                    basic_block.instructions.push(ArmIns::Load {
                        dst: "r0",
                        src: "=nlfmt",
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
            "intfmt: .asciz \"%d\"".to_string(),
            "nlfmt:  .asciz \"\\n\"".to_string(),
            "spacefmt:  .asciz \" \"".to_string(),
            ".text".to_string(),
            ".global main".to_string(),
            ".extern printf".to_string(),
            ".syntax unified".to_string(),
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
            if rhs_offsets.len() != lhs_offsets.len() {
                panic!("Not ready to compile dyadic op with LHS/RHS of differing lengths.")
            }
            for offset_idx in 0..rhs_offsets.len() {
                basic_block.instructions.push(ArmIns::LoadOffset {
                    dst: "r3",
                    src: "fp",
                    offsets: vec![*lhs_offsets.get(offset_idx).unwrap()]
                });
                basic_block.instructions.push(ArmIns::LoadOffset {
                    dst: "r4",
                    src: "fp",
                    offsets: vec![*rhs_offsets.get(offset_idx).unwrap()]
                });
                match verb {
                    DyadicVerb::Plus => {
                        basic_block.instructions.push(ArmIns::Add {
                            dst: "r4",
                            src: "r4",
                            add: "r3"
                        });
                    },
                    DyadicVerb::Times => {
                        basic_block.instructions.push(ArmIns::Multiply {
                            dst: "r4",
                            src: "r4",
                            mul: "r3"
                        });
                    },
                    _ => panic!("Not ready to compile dyadic verb: {:?}", verb)
                }
                basic_block.instructions.push(ArmIns::StoreOffset {
                    src: "r4",
                    dst: "fp",
                    offsets: vec![*lhs_offsets.get(offset_idx).unwrap()]
                });
            }
            lhs_offsets     // we are storing in LHS offsets, hence we return LHS offsets
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
        _ => panic!("Not ready to compute frame size of expr: {:?}", expr)
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<dyn (::Backend)>, String> {
    Ok(Box::new(ARMBackend {}))
}
