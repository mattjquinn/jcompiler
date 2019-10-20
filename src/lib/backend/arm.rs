use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;

use itertools::free::join;
use parser::{AstNode, MonadicVerb};

pub struct ARMBackend {}

#[derive(Debug)]
struct ValueRef {
    offset: i32,
}

#[derive(Debug)]
struct BasicBlock {
    base_offset: i32,
    instructions: Vec<ArmIns>,
    value_refs: Vec<ValueRef>,
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
            ArmIns::Multiply { dst, src, mul } => {
                f.write_str(format!("mul {}, {}, {}", dst, src, mul).as_str())
            }
            ArmIns::Nop => f.write_str("nop")
        }
    }
}

impl BasicBlock {
    fn new() -> BasicBlock {
        BasicBlock {
            base_offset: 0,
            instructions: Vec::new(),
            value_refs: Vec::new(),
        }
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
                    let mut basic_block = BasicBlock::new();
                    compile_expr(&mut basic_block, expr);

                    for value_ref in basic_block.value_refs.iter().rev() {
                        basic_block.instructions.push(ArmIns::Load {
                            dst: "r0",
                            src: "=intfmt",
                        });
                        basic_block.instructions.push(ArmIns::LoadOffset {
                            dst: "r1",
                            src: "sp",
                            offsets: vec![value_ref.offset],
                        });
                        basic_block
                            .instructions
                            .push(ArmIns::BranchAndLink { addr: "printf" });

                        // Multiple printed terms are separated by space, except for the last item
                        // TODO: This is a terrible way to check if we have iterated
                        // to the "final" value ref in the list
                        if value_ref.offset != 0 {
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

                    // TODO: This needs to be done within a "cleanup" method of BasicBlock
                    basic_block.instructions.push(ArmIns::AddImm {
                        dst: "sp",
                        src: "sp",
                        imm: basic_block
                            .value_refs
                            .last()
                            // TODO: Obviously this +4 needs to go (be more clear)
                            .map_or(0, |v| v.offset + 4),
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

fn compile_expr(basic_block: &mut BasicBlock, expr: &AstNode) {
    match expr {
        parser::AstNode::Integer(int) => {
            basic_block.instructions.push(ArmIns::MoveImm {
                dst: "r7",
                imm: *int,
            });
            basic_block.instructions.push(ArmIns::SubImm {
                dst: "sp",
                src: "sp",
                imm: 4,
            });
            basic_block.instructions.push(ArmIns::Store {
                dst: "sp",
                src: "r7",
            });
            basic_block.value_refs.push(ValueRef {
                offset: basic_block.base_offset,
            });
            basic_block.base_offset += 4;
        }
        parser::AstNode::Terms(terms) => {
            for term in terms {
                compile_expr(basic_block, term);
            }
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            basic_block.instructions.push(ArmIns::Nop);
            compile_expr(basic_block, expr);
            basic_block.instructions.push(ArmIns::Nop);
            for value_ref in basic_block.value_refs.iter().rev() {
                basic_block.instructions.push(ArmIns::LoadOffset {
                    dst: "r4",
                    src: "sp",
                    offsets: vec![value_ref.offset]
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
                    dst: "sp",
                    offsets: vec![value_ref.offset]
                });
            }
            basic_block.instructions.push(ArmIns::Nop);
        }
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<::Backend>, String> {
    Ok(Box::new(ARMBackend {}))
}
