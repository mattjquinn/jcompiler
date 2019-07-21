use parser;
use getopts::{Options,Matches};

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
        Ok(())
    }
}
pub fn register_cli_options(_options : &mut Options) {}

pub fn init_from_cli_options(_matches : &Matches) -> Result<Box<::Backend>, String> {
    Ok(Box::new(X86Backend{}))
}
