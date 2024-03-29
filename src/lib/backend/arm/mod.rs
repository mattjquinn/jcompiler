use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;

use self::compiler::{compile_expr, BasicBlock, GlobalContext};
use self::values::TypedValue;

mod compiler;
mod instructions;
mod ir;
mod macros;
mod memory;
mod registers;
mod values;

pub struct ARMBackend {}

impl ::Backend for ARMBackend {
    fn compile_ast(
        &self,
        _path: &str,
        ast: &[parser::AstNode],
        _do_report_mem_usage: bool,
        _do_verbose: bool,
        output_path: String,
    ) -> Result<(), String> {
        for astnode in ast {
            println!("{:?}", astnode);
        }

        let assembly_filename = format!("{}.s", output_path);
        let mut assembly_file =
            File::create(&assembly_filename).expect("Intermediate file in which to write assembly");
        println!("Writing assembly file to {}", &assembly_filename);

        let mut basic_blocks = Vec::new();
        let mut globalctx = GlobalContext::new();

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut basic_block = BasicBlock::new();
                    let values = compile_expr(&mut globalctx, &mut basic_block, expr);

                    match &**expr {
                        // top-level global assignments aren't printed
                        parser::AstNode::GlobalVarAssgmt { ident: _, expr: _ } => (),
                        _ => jprint_value(&values, &mut basic_block),
                    }
                    basic_block.cleanup();
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
            "\t.arch armv7-a",
            "\t.text",
            "\t.align 1",
            "\t.global main",
            "\t.syntax unified",
            "\t.thumb",
            "\t.thumb_func",
            "\t.fpu vfpv3-d16",
            "\t.type\tmain, %function",
            "main:",
            "\tpush\t{ip, lr}",
        ];
        for instr in preamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }
        globalctx.write_preamble_to_file(&mut assembly_file);
        for basic_block in &basic_blocks {
            basic_block.write_instructions_to_file(&mut assembly_file);
        }
        globalctx.write_postamble_to_file(&mut assembly_file);
        let postamble = vec!["\tpop\t{ip, pc}", ".L3:", "\t.align\t3"];
        for instr in postamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }
        globalctx.write_double_constant_pool_to_file(&mut assembly_file);
        let args = vec![
            "assemble-and-link-armv7.sh",
            &assembly_filename[..],
            &output_path,
        ];
        ::shell::run_shell_command("sh", &args)?;

        Ok(())
    }
}

fn jprint_value(values: &[Box<dyn TypedValue>], basic_block: &mut BasicBlock) {
    for (idx, value) in values.iter().enumerate() {
        // Multiple printed terms are separated by space, except for the last item
        let flags = match idx != values.len() - 1 {
            true => 1,
            false => 2,
        };
        value.print(flags, basic_block);
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<dyn (::Backend)>, String> {
    Ok(Box::new(ARMBackend {}))
}
