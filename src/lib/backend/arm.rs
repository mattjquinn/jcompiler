use parser;
use backend;
use getopts::{Options,Matches};
use std::io::Write;
use std::fs::File;

use tempfile::NamedTempFile;
use parser::AstNode;
use backend::arm::CompilationUnit::{CompilerInstructions, CompilerInstruction};

pub struct ARMBackend {}

#[derive(Debug)]
enum CompilationUnit {
    Integer(Vec<String>),
    Terms(Vec<CompilationUnit>),

    CompilerInstruction(String),
    CompilerInstructions(Vec<String>)
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

        let mut units = vec![
            CompilerInstructions(vec![
                ".data".to_string(),
                "intfmt: .asciz \"%d\"".to_string(),
                "nlfmt:  .asciz \"\\n\"".to_string(),
                "spacefmt:  .asciz \" \"".to_string(),
                ".text".to_string(),
                ".global main".to_string(),
                ".extern printf".to_string(),
                "main:".to_string(),
                "push {ip, lr}".to_string(),
            ])
        ];

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    match compile_expr(expr) {
                        CompilationUnit::Terms(terms_units) => {
                            let mut i : usize = 1;
                            let len = terms_units.len();
                            for term_unit in terms_units {
                                units.push(term_unit);

                                // Multiple printed terms are separated by space,
                                // except for the last item
                                if i < len {
                                    let mut instrs = Vec::new();
                                    instrs.push("ldr r0, =spacefmt".to_string());
                                    instrs.push("bl printf".to_string());
                                    units.push(CompilerInstructions(instrs))
                                }
                                i += 1;
                            }
                        },
                        unit => units.push(unit)
                    }

                    // All printed expressions are terminated with a newline.
                    let mut instrs = Vec::new();
                    instrs.push("ldr r0, =nlfmt".to_string());
                    instrs.push("bl printf".to_string());
                    units.push(CompilerInstructions(instrs))
                },
                _ => panic!("Not ready to compile top-level AST node: {:?}", astnode)
            }
        }

        units.push(CompilerInstruction("pop {ip, pc}".to_string()));

        let mut instrs = Vec::new();
        compilation_units_to_instrs(&mut instrs, units);

        println!("Printing ARM...");
        for instr in &instrs {
            println!("{}", instr);
        }

        println!("Writing ARM...");
        for instr in &instrs {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }

        let args = vec!["assemble-and-link-armv7.sh", &assembly_filename[..], &output_path];
        ::shell::run_shell_command("sh", &args)?;

        Ok(())
    }
}

fn compile_expr(expr : &AstNode) -> CompilationUnit {
    match expr {
        parser::AstNode::Integer(int) => {
            let mut instrs = Vec::new();
            instrs.push("ldr r0, =intfmt".to_string());
            instrs.push(format!("mov r1, #{}", &int));
            instrs.push("bl printf".to_string());
            CompilationUnit::Integer(instrs)
        },
        parser::AstNode::Terms(terms) => {
            let mut units = Vec::new();
            for term in terms {
                units.push(compile_expr(term));
            }
            CompilationUnit::Terms(units)
        },
        _ => panic!("Not ready to compile expression: {:?}", expr)
    }
}

fn compilation_units_to_instrs(instrs : &mut Vec<String>, units : Vec<CompilationUnit>) {
    for unit in units {
        match unit {
            CompilerInstruction(instr) =>
                instrs.push(instr),
            CompilerInstructions(c_instrs)
                | CompilationUnit::Integer(c_instrs) => {
                instrs.extend(c_instrs)
            },
            CompilationUnit::Terms(units) => {
                compilation_units_to_instrs(instrs, units)
            }
        }
    }
}

pub fn register_cli_options(_options : &mut Options) {}

pub fn init_from_cli_options(_matches : &Matches) -> Result<Box<::Backend>, String> {
    Ok(Box::new(ARMBackend {}))
}
