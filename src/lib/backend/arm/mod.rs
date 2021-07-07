use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;

use self::instructions::{ArmIns};
use self::compiler::{GlobalContext, BasicBlock, TypedValue, compile_expr};
use backend::arm::registers::{CoreRegister, ExtensionRegister};

mod instructions;
mod macros;
mod compiler;
mod registers;
mod ir;

pub struct ARMBackend {}

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
                        _ => {
                            jprint_value(&values, &mut basic_block);

                            // All printed expressions are terminated with a newline followed by three spaces (per ijconsole)
                            basic_block.push(ArmIns::Load {
                                dst: CoreRegister::R0,
                                src: "=line_end_nl_fmt".to_string(),
                            });
                            basic_block.push(ArmIns::BranchAndLink { addr: "printf" });
                        }
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
            ".arch armv7-a",
            ".data",
            "line_end_nl_fmt:  .asciz \"\\n\"",
            "space_fmt:  .asciz \" \"",
            ".text",
            ".global main",
            ".syntax unified",
            "main:",
            "push {ip, lr}",
        ];
        for instr in preamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }
        globalctx.write_preamble_to_file(&mut assembly_file);
        for basic_block in &basic_blocks {
            basic_block.write_instructions_to_file(&mut assembly_file);
        }
        globalctx.write_postamble_to_file(&mut assembly_file);
        let mut postamble = Vec::new();
        postamble.push("pop {ip, pc}".to_string());
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

fn jprint_value(values: &Vec<TypedValue>, basic_block: &mut BasicBlock) {
    for (idx, value) in values.iter().enumerate() {
        match &value {
            TypedValue::Integer(pointer) => {
                pointer.read(CoreRegister::R0, basic_block);
                basic_block.push(ArmIns::BranchAndLink { addr: "jprint_int" });
            },
            TypedValue::Double { msw, lsw } => {
                // TODO: reclaim this stack entry after the call to print, we only use it to load register d0
                let sp_offset = basic_block.stack_allocate_width(8);
                msw.copy_to_stack_offset(sp_offset + 4, basic_block);
                lsw.copy_to_stack_offset(sp_offset, basic_block);
                basic_block.push(ArmIns::LoadExtensionRegisterWidth64 {
                    dst: ExtensionRegister::D0, src: CoreRegister::SP, offsets: vec![sp_offset] });
                basic_block.push(ArmIns::BranchAndLink { addr: "jprint_double" });
            }
        }
        // Multiple printed terms are separated by space, except for the last item
        if idx != values.len() - 1 {
            basic_block.push(ArmIns::Load {
                dst: CoreRegister::R0,
                src: "=space_fmt".to_string(),
            });
            basic_block.push(ArmIns::BranchAndLink { addr: "printf" });
        }
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<dyn (::Backend)>, String> {
    Ok(Box::new(ARMBackend {}))
}
