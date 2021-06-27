use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;
use std::collections::{HashMap, HashSet};

use self::instructions::{ArmIns};
use self::support::{GlobalContext, BasicBlock, Offset, Type};
use self::compiler::{register_globals, compute_stack_size, compile_expr};
use backend::arm::registers::ArmRegister;

mod instructions;
mod macros;
mod support;
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
        let mut globalctx = {
            // TODO: We can get rid of a lot of this now, save for the fact
            // that there is a weird side-effect somewhere that requires us to keep this around
            // for tests to pass.
            let mut global_idents = HashSet::new();
            let mut ident_type_map = HashMap::new();
            for astnode in ast {
                register_globals(&astnode, &mut global_idents, &mut ident_type_map);
            }
            GlobalContext {
                global_ident_to_offsets: HashMap::new(),
            }
        };

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut basic_block = BasicBlock::new(compute_stack_size(expr));
                    let mut global_basic_block = BasicBlock::new(0);
                    let val_offsets = compile_expr(&mut globalctx, &mut global_basic_block, &mut basic_block, expr);

                    match &**expr {
                        // top-level global assignments aren't printed
                        parser::AstNode::GlobalVarAssgmt { ident: _, expr: _ } => (),
                        _ => {
                            jprint_offset(&val_offsets, &globalctx, &mut basic_block);

                            // All printed expressions are terminated with a newline followed by three spaces (per ijconsole)
                            basic_block.push(ArmIns::Load {
                                dst: ArmRegister::R0,
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
        let mut preamble = vec![
            ".arch armv7-a".to_string(),
            ".data".to_string(),
            "pos_int_fmt: .asciz \"%d\"".to_string(),
            "neg_int_fmt: .asciz \"_%d\"".to_string(),
            "pos_double_fmt: .asciz \"%g\"".to_string(),
            "neg_double_fmt: .asciz \"_%g\"".to_string(),
            "line_end_nl_fmt:  .asciz \"\\n\"".to_string(),
            "space_fmt:  .asciz \" \"".to_string()];
        globalctx.emit_preamble_entries(&mut preamble);
        preamble.extend(vec![
            ".text".to_string(),
            ".global main".to_string(),
            ".extern printf".to_string(),
            ".syntax unified".to_string(),
            // printing related functions
            // TODO: combine these into their common parts
            // === INTEGER ==============================================
            "jprint_int:".to_string(),
            "push {lr}".to_string(),
            "cmp r1, #0".to_string(),
            "blt jprint_int_neg".to_string(),
            "ldr r0, =pos_int_fmt".to_string(),
            "jprint_int_main:".to_string(),
            "bl printf".to_string(),
            "pop {lr}".to_string(),
            "bx lr".to_string(),
            "jprint_int_neg:".to_string(),
            "ldr r0, =neg_int_fmt".to_string(),
            "rsblt r1, r1, #0".to_string(), // takes abs value of r1
            "bl jprint_int_main".to_string(),
            // === DOUBLE ==============================================
            "jprint_double:".to_string(),
            "push {lr}".to_string(),
            // If the double precision value has no fractional bits set,
            // print it as an integer in an effort to match J's behavior.
            "cmp r3, #0".to_string(), // are there are any significand bits set in the LSW?
            "bne jprint_sign_check".to_string(),
            "mov r8, #255".to_string(), // partial mask: 0xFF
            "orr r8, r8, #65280".to_string(), // partial mask: 0xFF00
            "orr r8, r8, #983040".to_string(), // partial mask: 0xF0000
            "tst r2, r8".to_string(), // apply the full mask (0xFFFFF) to the LSW
            "bne jprint_sign_check".to_string(), // if masked bits are clear, Z flag will be 1; we branch if Z flag is 0
            "mov r0, r3".to_string(),
            "mov r1, r2".to_string(),
            "bl __aeabi_d2iz".to_string(), // convert the integral part (exponent) to an integer
            "mov r1, r0".to_string(),  // the integer result is in r0
            "bl jprint_int".to_string(), // print as an integer
            "pop {lr}".to_string(),
            "bx lr".to_string(),
            // we jump here if the value does in fact have a fractional part
            "jprint_sign_check:".to_string(),
            "and r0, r2, #0x80000000".to_string(),
            "cmp r0, #0x80000000".to_string(),  // true if MSB of MSW is negative
            "beq jprint_double_neg".to_string(),
            "ldr r0, =pos_double_fmt".to_string(),
            "jprint_double_main:".to_string(),
            "bl printf".to_string(),
            "pop {lr}".to_string(),
            "bx lr".to_string(),
            "jprint_double_neg:".to_string(),
            "ldr r0, =neg_double_fmt".to_string(),
            "bic r2, r2, 2147483648".to_string(), // clear the sign bit in MSW
            "bl jprint_double_main".to_string(),
            // main
            "main:".to_string(),
            "push {ip, lr}".to_string(),
        ]);
        for instr in preamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }
        for basic_block in &basic_blocks {
            basic_block.write_instructions_to_file(&mut assembly_file);
        }
        let mut postamble = Vec::new();
        postamble.push("pop {ip, pc}".to_string());
        globalctx.emit_postamble_entries(&mut postamble);
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

fn jprint_offset(val_offsets: &Vec<Offset>, globalctx: &GlobalContext, basic_block: &mut BasicBlock) {
    for (idx, offset) in val_offsets.iter().enumerate() {
        match offset {
            Offset::Frame(Type::Integer, i) => {
                basic_block.push(ArmIns::LoadOffset {
                    dst: ArmRegister::R1,
                    src: ArmRegister::FP,
                    offsets: vec![*i],
                });
                basic_block.push(
                    ArmIns::BranchAndLink { addr: "jprint_int" });
            },
            Offset::Frame(Type::Double, i) => {
                // the MSW is expected in r2
                basic_block.push(ArmIns::LoadOffset {
                    dst: ArmRegister::R2,
                    src: ArmRegister::FP,
                    offsets: vec![*i]
                });
                // the LSW is expected in r3
                basic_block.push(ArmIns::LoadOffset {
                    dst: ArmRegister::R3,
                    src: ArmRegister::FP,
                    offsets: vec![*i + 4]  // if we had DoubleOffset(msw, lsw) we wouldn't need the manual + 4 here
                });
                basic_block.push(
                    ArmIns::BranchAndLink { addr: "jprint_double" });
            }
            Offset::Global(Type::Integer, ident) => {
                basic_block.push(ArmIns::Load {
                    dst: ArmRegister::R1,
                    src: format!(".{}", ident).to_string()
                });
                basic_block.push(ArmIns::Load {
                    dst: ArmRegister::R1,
                    src: "[r1]".to_string(),
                });
                basic_block.push(
                    ArmIns::BranchAndLink { addr: "jprint_int" });
            },
            Offset::Global(Type::Double, ident) => {
                // the MSW is expected in r2
                basic_block.push(ArmIns::Load {
                    dst: ArmRegister::R2,
                    // no "_double" prefix because it is already included in the ident
                    src: format!(".{}_msw", ident).to_string()
                });
                basic_block.push(ArmIns::Load {
                    dst: ArmRegister::R2,
                    src: "[r2]".to_string(),
                });
                // the LSW is expected in r3
                basic_block.push(ArmIns::Load {
                    dst: ArmRegister::R3,
                    // no "_double" prefix because it is already included in the ident
                    src: format!(".{}_lsw", ident).to_string()
                });
                basic_block.push(ArmIns::Load {
                    dst: ArmRegister::R3,
                    src: "[r3]".to_string(),
                });
                basic_block.push(
                    ArmIns::BranchAndLink { addr: "jprint_double" });
            },
            /* fall-through for global offsets */
            Offset::Global(ty, i) => {
                match ty {
                    Type::Array(_) => {
                        let offsets = globalctx.global_ident_to_offsets.get(i).unwrap();
                        jprint_offset(offsets, globalctx, basic_block);
                    },
                    _ => panic!("TODO: Unexpected Global offset: {:?}", offset)
                }
            },
            _ => panic!("TODO: unexpected offset used with jprint: {:?}", offset)
        };

        // Multiple printed terms are separated by space, except for the last item
        if idx != val_offsets.len() - 1 {
            basic_block.push(ArmIns::Load {
                dst: ArmRegister::R0,
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
