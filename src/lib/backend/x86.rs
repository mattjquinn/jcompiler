use parser;
use backend;
use getopts::{Options,Matches};
use std::io::Write;
use std::fs::File;

use tempfile::NamedTempFile;

pub struct X86Backend {}

impl ::Backend for X86Backend {
    fn compile_ast(&self,
                   path: &str,
                   ast: &Vec<parser::AstNode>,
                   do_report_mem_usage: bool,
                   do_verbose: bool,
                   output_path: String
    ) -> Result<(), String> {

        println!("TODO: Compile this AST to x86:");
        for astnode in ast {
            println!("{:?}", astnode);
        }

        let assembly_filename = format!("{}.s", output_path);
        let assembly_file = File::create(&assembly_filename)
            .expect("Intermediate file in which to write assembly");
        println!("Writing assembly file to {}", &assembly_filename);

        let instrs = vec![
            "global _start",
            "section .text",

            "_start:",
            "   mov rax, 1        ; write(",
            "   mov rdi, 1        ;   STDOUT_FILENO,",
            "   mov rsi, msg      ;   Hello, world!,",
            "   mov rdx, msglen   ;   sizeof(Hello, world!)",
            "   syscall           ; );",

            "   mov rax, 60       ; exit(",
            "   mov rdi, 0        ;   EXIT_SUCCESS",
            "   syscall           ; );",

            "section .rodata",
            "   msg: db \"Hello, world!\", 10",
            "   msglen: equ $ - msg",
        ];

        for instr in instrs {
            writeln!(&assembly_file, "{}", instr);
        }

        let object_filename = format!("{}.o", assembly_filename);
        let nasm_args = vec!["-f", "elf64", "-o", &object_filename, &assembly_filename];
        ::shell::run_shell_command("nasm", &nasm_args)?;

        let ld_args = vec!["-o", &output_path, &object_filename];
        ::shell::run_shell_command("ld", &ld_args)?;

        Ok(())
    }
}

pub fn register_cli_options(_options : &mut Options) {}

pub fn init_from_cli_options(_matches : &Matches) -> Result<Box<::Backend>, String> {
    Ok(Box::new(X86Backend{}))
}
