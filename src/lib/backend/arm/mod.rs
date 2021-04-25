use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;
use std::collections::{HashMap, HashSet};

use self::instructions::{ArmIns};
use self::support::{GlobalContext, BasicBlock, Offset, Type};
use self::compiler::{register_globals, compute_frame_size, compile_expr};

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
        let assembly_file =
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
            let mut globals_table : HashMap<String, i32> = HashMap::new();
            if global_idents.len() > 0 {
                // Assuming all values are 32-bit integers for now:
                let mut global_bb = BasicBlock::new((global_idents.len() * 4) as i32);
                for ident in global_idents {
                    globals_table.insert(ident.clone(), global_bb.stack_allocate_int());
                }
                basic_blocks.push(global_bb);
                println!("Globals table: {:?}", globals_table);
            }
            GlobalContext {
                global_ident_to_offsets: HashMap::new(),
            }
        };

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut basic_block = BasicBlock::new(compute_frame_size(expr));
                    let mut global_basic_block = BasicBlock::new(0);
                    let val_offsets = compile_expr(&mut globalctx, &mut global_basic_block, &mut basic_block, expr);

                    match &**expr {
                        // top-level global assignments aren't printed
                        parser::AstNode::GlobalVarAssgmt { ident: _, expr: _ } => (),
                        _ => {
                            jprint_offset(&val_offsets, &globalctx, &mut basic_block);

                            // All printed expressions are terminated with a newline followed by three spaces (per ijconsole)
                            basic_block.instructions.push(ArmIns::Load {
                                dst: "r0".to_string(),
                                src: "=line_end_nl_fmt".to_string(),
                            });
                            basic_block
                                .instructions
                                .push(ArmIns::BranchAndLink { addr: "printf" });
                        }
                    }

                    // Erases the frame for this block.
                    let mut added = 0;
                    while added < basic_block.frame_size {
                        let mut to_add = basic_block.frame_size - added;
                        // ctest_mixed_adds_mults appears to be blowing the immediate width...
                        if to_add > 256 {
                            to_add = 256;
                        }
                        basic_block.instructions.push(ArmIns::AddImmDeprecated {
                            dst: "fp",
                            src: "fp",
                            imm: to_add
                        });
                        added += to_add;
                    }
                    // Stack pointer must be moved up as well.
                    basic_block.instructions.push(ArmIns::Move {
                        dst: "sp",
                        src: "fp"
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
        let mut preamble = vec![
            ".arch armv7-a".to_string(),
            ".data".to_string(),
            "pos_int_fmt: .asciz \"%d\"".to_string(),
            "neg_int_fmt: .asciz \"_%d\"".to_string(),
            "pos_double_fmt: .asciz \"%g\"".to_string(),
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
            // TODO: Eventually support negatives using "cmp"
            "jprint_double:".to_string(),
            "push {lr}".to_string(),
            // printf expects r2 to contain MSW, r3 to contain LSW
            "ldr r0, =pos_double_fmt".to_string(),
            "jprint_double_main:".to_string(),
            "bl printf".to_string(),
            "pop {lr}".to_string(),
            "bx lr".to_string(),
            // main
            "main:".to_string(),
            "push {ip, lr}".to_string(),
        ]);
        for instr in preamble {
            writeln!(&assembly_file, "{}", instr).expect("write failure");
        }
        for basic_block in &basic_blocks {
            for instr in &basic_block.instructions {
                writeln!(&assembly_file, "{}", instr).expect("write failure");
            }
        }
        let mut postamble = Vec::new();
        globalctx.cleanup_stack(&mut postamble);
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
            Offset::Stack(ty, i) if *ty == Type::Integer => {
                basic_block.instructions.push(ArmIns::LoadOffset {
                    dst: "r1",
                    src: "fp",
                    offsets: vec![*i],
                });
                basic_block.instructions.push(
                    ArmIns::BranchAndLink { addr: "jprint_int" });
            },
            Offset::Global(ty, ident) if *ty == Type::Integer => {
                basic_block.instructions.push(ArmIns::Load {
                    dst: "r1".to_string(),
                    src: format!(".{}", ident).to_string()
                });
                basic_block.instructions.push(ArmIns::Load {
                    dst: "r1".to_string(),
                    src: "[r1]".to_string(),
                });
                basic_block.instructions.push(
                    ArmIns::BranchAndLink { addr: "jprint_int" });
            },
            Offset::Global(ty, ident) if *ty == Type::Double => {
                // the LSW is expected in r2
                // TODO: obviously not all LSWs are 0; this is an invariant
                // for now in the code section responsible for loading double prec literals
                basic_block.instructions.push(ArmIns::MoveImmDeprecated {
                    dst: "r2", imm: 0
                });
                // the MSW is expected in r3
                basic_block.instructions.push(ArmIns::Load {
                    dst: "r3".to_string(),
                    src: format!(".{}", ident).to_string()
                });
                basic_block.instructions.push(ArmIns::Load {
                    dst: "r3".to_string(),
                    src: "[r3]".to_string(),
                });
                basic_block.instructions.push(
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
            basic_block.instructions.push(ArmIns::Load {
                dst: "r0".to_string(),
                src: "=space_fmt".to_string(),
            });
            basic_block
                .instructions
                .push(ArmIns::BranchAndLink { addr: "printf" });
        }
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<dyn (::Backend)>, String> {
    Ok(Box::new(ARMBackend {}))
}
