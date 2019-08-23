use parser;
use backend;
use getopts::{Options,Matches};
use std::io::Write;
use std::fs::File;

use tempfile::NamedTempFile;
use parser::AstNode;
use std::ops::Add;

pub struct ARMBackend {}

#[derive(Debug)]
struct ValueRef {
    offset: i32,
}

#[derive(Debug)]
struct BasicBlock {
    base_offset: i32,
    instructions: Vec<String>,
    value_refs: Vec<ValueRef>,
}

impl BasicBlock {
    fn new() -> BasicBlock {
        BasicBlock {
            base_offset: 0,
            instructions : Vec::new(),
            value_refs : Vec::new()
        }
    }
}

impl ::Backend for ARMBackend {
    fn compile_ast(&self,
                   path: &str,
                   ast: &Vec<parser::AstNode>,
                   do_report_mem_usage: bool,
                   do_verbose: bool,
                   output_path: String
    ) -> Result<(), String> {

        for astnode in ast {
            println!("{:?}", astnode);
        }

        let assembly_filename = format!("{}.s", output_path);
        let assembly_file = File::create(&assembly_filename)
            .expect("Intermediate file in which to write assembly");
        println!("Writing assembly file to {}", &assembly_filename);

        let mut basic_blocks = Vec::new();

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut basic_block = BasicBlock::new();
                    compile_expr(&mut basic_block, expr);

                    for value_ref in basic_block.value_refs.iter().rev() {
                        basic_block.instructions.push("ldr r0, =intfmt".to_string());
                        basic_block.instructions.push(format!("ldrb r1, [sp, # {}]", value_ref.offset));
                        basic_block.instructions.push("bl printf".to_string());

                        // Multiple printed terms are separated by space, except for the last item
                        // TODO: This is a terrible way to check if we have iterated
                        // to the "final" value ref in the list
                        if value_ref.offset != 0 {
                            basic_block.instructions.push("ldr r0, =spacefmt".to_string());
                            basic_block.instructions.push("bl printf".to_string());
                        }
                    }

                    // All printed expressions are terminated with a newline.
                    basic_block.instructions.push("ldr r0, =nlfmt".to_string());
                    basic_block.instructions.push("bl printf".to_string());

                    // TODO: This needs to be done within a "cleanup" method of BasicBlock
                    basic_block.instructions.push(
                        format!("add sp, sp, # {}",
                                basic_block.value_refs.last()
                                    // TODO: Obviously this +4 needs to go (be more clear)
                                    .map_or(0,|v| v.offset + 4)));

                    basic_blocks.push(basic_block);
                },
                _ => panic!("Not ready to compile top-level AST node: {:?}", astnode)
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


        let args = vec!["assemble-and-link-armv7.sh", &assembly_filename[..], &output_path];
        ::shell::run_shell_command("sh", &args)?;

        Ok(())
    }
}

fn compile_expr(basic_block : &mut BasicBlock, expr : &AstNode) {
    match expr {
        parser::AstNode::Integer(int) => {
            basic_block.instructions.push(format!("mov r7, #{}", &int));
            basic_block.instructions.push(format!("sub sp, sp, #4"));
            basic_block.instructions.push(format!("str r7, [sp]"));
            basic_block.value_refs.push(ValueRef {
                offset: basic_block.base_offset
            });
            basic_block.base_offset += 4;
        },
        parser::AstNode::Terms(terms) => {
            for term in terms {
                compile_expr(basic_block, term);
            }
        },
        _ => panic!("Not ready to compile expression: {:?}", expr)
    }
}

pub fn register_cli_options(_options : &mut Options) {}

pub fn init_from_cli_options(_matches : &Matches) -> Result<Box<::Backend>, String> {
    Ok(Box::new(ARMBackend {}))
}
