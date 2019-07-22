use parser;
use backend;
use getopts::{Options,Matches};
use std::io::Write;
use std::fs::File;

use tempfile::NamedTempFile;

pub struct ARMBackend {}

impl ::Backend for ARMBackend {
    fn compile_ast(&self,
                   path: &str,
                   ast: &Vec<parser::AstNode>,
                   do_report_mem_usage: bool,
                   do_verbose: bool,
                   output_path: String
    ) -> Result<(), String> {

        println!("TODO: Compile this AST to ARM:");
        for astnode in ast {
            println!("{:?}", astnode);
        }

        let assembly_filename = format!("{}.s", output_path);
        let assembly_file = File::create(&assembly_filename)
            .expect("Intermediate file in which to write assembly");
        println!("Writing assembly file to {}", &assembly_filename);

        let instrs = vec![
            ".text",
            ".global main",
            ".extern printf",
            "main:",
            "       push {ip, lr}",

            "       ldr r0, =string",
            "       mov r1, #1029",
            "       bl printf",

            "       pop {ip, pc}",

            ".data",
            "string: .asciz \"The number is: %d\n\"",
        ];

        for instr in instrs {
            writeln!(&assembly_file, "{}", instr);
        }

        let args = vec!["assemble-and-link-armv7.sh", &assembly_filename[..], &output_path];
        ::shell::run_shell_command("sh", &args)?;

        Ok(())
    }
}

pub fn register_cli_options(_options : &mut Options) {}

pub fn init_from_cli_options(_matches : &Matches) -> Result<Box<::Backend>, String> {
    Ok(Box::new(ARMBackend {}))
}
