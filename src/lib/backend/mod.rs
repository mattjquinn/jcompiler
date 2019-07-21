pub mod llvm;
pub mod x86;

use parser::AstNode;
use getopts::{Options,Matches};

pub trait Backend {
    fn compile_ast(&self,
                   path : &str,
                   ast : &Vec<AstNode>,
                   do_report_mem_usage : bool,
                   do_verbose : bool,
                   output_path: String
    ) -> Result<(), String>;
}

pub fn register_cli_options(options: &mut Options) {
    options.reqopt(
        "b",
        "backend",
        "specifies the compiler backend to use", "llvm|x86");

    llvm::register_cli_options(options);
    x86::register_cli_options(options);
}

pub fn init_from_cli_options(matches: &Matches) -> Result<Box<Backend>, String> {
    match matches.opt_str("backend") {
        Some(ref choice) if &choice[..] == "llvm" =>
            llvm::init_from_cli_options(&matches),
        Some(ref choice) if &choice[..] == "x86" =>
            x86::init_from_cli_options(&matches),
        Some(choice) => Err(format!("Unrecognized choice of backend: {}", choice)),
        None => Err("No choice of backend was specified".to_string())
    }
}

fn convert_io_error<T>(result: Result<T, std::io::Error>) -> Result<T, String> {
    match result {
        Ok(value) => Ok(value),
        Err(e) => Err(format!("{}", e)),
    }
}

