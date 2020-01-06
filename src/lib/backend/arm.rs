use getopts::{Matches, Options};
use parser;
use std::fs::File;
use std::io::Write;
use std::collections::{HashMap, HashSet};

use itertools::free::join;
use parser::{AstNode, MonadicVerb, DyadicVerb};
use itertools::Itertools;
use itertools::EitherOrBoth::{Both, Left, Right};
use std::fmt::{Formatter, Error};
use std::cmp::max;
use ieee754::Ieee754;

pub struct ARMBackend {}

#[derive(Debug)]
enum Offset {
    Stack(Type, i32),
    Global(Type, String),
}

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Offset::Stack(ty, i) => f.write_fmt(format_args!(
                "Stack<offset={},type={}>", ty, i)),
            Offset::Global(ty, s) => f.write_fmt(format_args!(
                "Global<offset={},type={}>", ty, s))
        }
    }
}

impl std::clone::Clone for Offset {
    fn clone(&self) -> Self {
        match self {
            Offset::Stack(ty, i) => Offset::Stack(ty.clone(), *i),
            Offset::Global(ty, s) => Offset::Global(ty.clone(), s.clone())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Integer,
    Float  // single precision
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        // TODO: there must be a more concise way to do this
        let s = match self {
            Type::Integer => "Integer",
            Type::Float => "Float",
        };
        f.write_fmt(format_args!("{}", s))
    }
}

#[derive(Debug)]
struct GlobalContext {
    globals_table: HashMap<String, i32>,
    ident_type_map: HashMap<String, Type>,
}

#[derive(Debug)]
struct BasicBlock {
    frame_pointer: i32,
    frame_size: i32,
    instructions: Vec<ArmIns>,
}

#[derive(Debug)]
enum ArmIns {
    Load { dst: String, src: String },
    Store { dst: String, src: String },
    LoadOffset { dst: &'static str, src: &'static str, offsets: Vec<i32> },
    StoreOffset { dst: &'static str, src: &'static str, offsets: Vec<i32> },
    BranchAndLink { addr: &'static str },
    Add { dst: &'static str, src: &'static str, add: &'static str },
    AddImm { dst: &'static str, src: &'static str, imm: i32 },
    Sub { dst: &'static str, src: &'static str, sub: &'static str },
    SubImm { dst: &'static str, src: &'static str, imm: i32 },
    Move { dst: &'static str, src: &'static str },
    MoveImm { dst: &'static str, imm: i32 },
    Multiply { dst: &'static str, src: &'static str, mul: &'static str },
    Compare { lhs: &'static str, rhs: &'static str },
    MoveLT { dst: &'static str, src: &'static str },
    MoveLE { dst: &'static str, src: &'static str },
    MoveGT { dst: &'static str, src: &'static str },
    MoveGE { dst: &'static str, src: &'static str },
    MoveNE { dst: &'static str, src: &'static str },
    MoveEQ { dst: &'static str, src: &'static str },
    Nop
}

impl std::fmt::Display for ArmIns {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), core::fmt::Error> {
        match self {
            ArmIns::Load { dst, src } =>
                f.write_str(format!("ldr {}, {}", dst, src).as_str()),
            ArmIns::Store { dst, src } =>
                f.write_str(format!("str {}, [{}]", src, dst).as_str()),
            ArmIns::LoadOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("ldr {}, [{}, {}]", dst, src, offset_str).as_str())
            }
            ArmIns::StoreOffset { dst, src, offsets } => {
                let offset_str = join(offsets, ", ");
                f.write_str(format!("str {}, [{}, {}]", src, dst, offset_str).as_str())
            }
            ArmIns::Compare { lhs, rhs } => {
                f.write_str(format!("cmp {}, {}", lhs, rhs).as_str())
            }
            ArmIns::MoveLT { dst, src } => {
                f.write_str(format!("movlt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveLE { dst, src } => {
                f.write_str(format!("movle {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGT { dst, src } => {
                f.write_str(format!("movgt {}, {}", dst, src).as_str())
            }
            ArmIns::MoveGE { dst, src } => {
                f.write_str(format!("movge {}, {}", dst, src).as_str())
            }
            ArmIns::MoveEQ { dst, src } => {
                f.write_str(format!("moveq {}, {}", dst, src).as_str())
            }
            ArmIns::MoveNE { dst, src } => {
                f.write_str(format!("movne {}, {}", dst, src).as_str())
            }
            ArmIns::BranchAndLink { addr } => f.write_str(format!("bl {}", addr).as_str()),
            ArmIns::Move { dst, src } =>
                f.write_str(format!("mov {}, {}", dst, src).as_str()),
            ArmIns::MoveImm { dst, imm } => f.write_str(format!("mov {}, {}", dst, imm).as_str()),
            ArmIns::AddImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("add {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("add {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::SubImm { dst, src, imm } => {
                if dst == src {
                    // ARM allows compressed format if source and destination are identical.
                    f.write_str(format!("sub {}, {}", src, imm).as_str())
                } else {
                    f.write_str(format!("sub {}, {}, {}", dst, src, imm).as_str())
                }
            }
            ArmIns::Sub { dst, src, sub } => {
                f.write_str(format!("sub {}, {}, {}", dst, src, sub).as_str())
            }
            ArmIns::Add { dst, src, add } => {
                f.write_str(format!("add {}, {}, {}", dst, src, add).as_str())
            }
            ArmIns::Multiply { dst, src, mul } => {
                f.write_str(format!("mul {}, {}, {}", dst, src, mul).as_str())
            }
            ArmIns::Nop => f.write_str("nop")
        }
    }
}

impl BasicBlock {
    fn new(frame_size: i32) -> BasicBlock {
        println!("Allocating new basic block with frame size {}", frame_size);
        let mut instructions = vec![
            // Frame pointer starts out at stack pointer.
            ArmIns::Move { dst: "fp", src: "sp" } ];
        let mut subbed = 0;
        while subbed < frame_size {
            let mut to_sub = frame_size - subbed;
            // ctest_mixed_adds_mults appears to be blowing the immediate width...
            if to_sub > 256 {
                to_sub = 256;
            }
            // Expand fp to size of frame.
            instructions.push(ArmIns::SubImm {
                dst: "fp",
                src: "fp",
                imm: to_sub
            });
            subbed += to_sub;
        }
        // Expand sp along with it.
        instructions.push(ArmIns::Move { dst: "sp", src: "fp" });
        BasicBlock {
            frame_size,
            frame_pointer: 0,
            instructions,
        }
    }

    // TODO: should be inaccessible to callers outside of this struct
    fn _stack_allocate(&mut self, width: i32) -> i32 {
        if self.frame_pointer + width > self.frame_size {
            panic!("Attempted to allocate entity of width {} which overflows frame size of {}; frame pointer is {}", width, self.frame_size, self.frame_pointer);
        }
        let offset = self.frame_pointer;
        self.frame_pointer += width;
        offset
    }

    fn stack_allocate_int(&mut self) -> i32 {
        let offset = self._stack_allocate(4);
        offset
    }

    fn stack_allocate_double(&mut self) -> i32 {
        let offset = self._stack_allocate(4);
        offset
    }
}

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
        let globalctx = {
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
                globals_table,
                ident_type_map
            }
        };

        for astnode in ast {
            match astnode {
                parser::AstNode::Print(expr) => {
                    let mut basic_block = BasicBlock::new(compute_frame_size(expr));
                    let val_offsets = compile_expr(&globalctx, &mut basic_block, expr);

                    match &**expr {
                        // top-level global assignments aren't printed
                        parser::AstNode::IsGlobal { ident: _, expr: _ } => (),
                        _ => {
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
                                    Offset::Global(ty, ident) if *ty == Type::Float => {
                                        // the LSW is expected in r2
                                        // TODO: obviously not all LSWs are 0; this is an invariant
                                        // for now in the code section responsible for loading double prec literals
                                        basic_block.instructions.push(ArmIns::MoveImm {
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

                                    }
                                    _ => panic!("TODO: support this offset for use with jprint: {:?}", offset)
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

                            // All printed expressions are terminated with a newline.
                            basic_block.instructions.push(ArmIns::Load {
                                dst: "r0".to_string(),
                                src: "=nl_fmt".to_string(),
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
                        basic_block.instructions.push(ArmIns::AddImm {
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
            "nl_fmt:  .asciz \"\\n\"".to_string(),
            "space_fmt:  .asciz \" \"".to_string()];
        for ident in globalctx.globals_table.keys() {
            preamble.push(format!("{}: .word 0", ident));
        }
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
        if globalctx.globals_table.len() > 0 {
            // Erases the global frame
            postamble.push(format!("{}", ArmIns::AddImm {
                dst:"fp",
                src:"fp",
                imm: (globalctx.globals_table.len() * 4) as i32
            }));
            // Stack pointer must be moved up as well.
            postamble.push(format!("{}",ArmIns::Move {
                dst: "sp",
                src: "fp"
            }));
        }
        postamble.push("pop {ip, pc}".to_string());
        for ident in globalctx.globals_table.keys() {
            postamble.push(format!(".{}: .word {}", ident, ident));
        }
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

fn unify_types(l_type: &Type, r_type: &Type) -> Type {
    match (l_type, r_type) {
        (Type::Integer, Type::Integer) => Type::Integer,
        (Type::Float, Type::Float) => Type::Float,
        _ => unimplemented!("TODO: Support unification of type {} with {}", l_type, r_type)
    }
}

fn compile_expr(globalctx: &GlobalContext, basic_block: &mut BasicBlock, expr: &AstNode) -> Vec<Offset> {
    match expr {
        parser::AstNode::Integer(int) => {
            basic_block.instructions.push(ArmIns::MoveImm {
                dst: "r7",
                imm: *int,
            });
            let offset = basic_block.stack_allocate_int();
            basic_block.instructions.push(ArmIns::StoreOffset {
                dst: "fp",
                src: "r7",
                offsets: vec![offset],
            });
            vec![Offset::Stack(Type::Integer, offset)]
        },
        parser::AstNode::DoublePrecisionFloat(num) => {
            let bits = num.bits();
            let hex_rep = format!("{:016x}", bits);
            println!("IEEE754 double hex representation: {}", hex_rep);

            if bits & 0x00000000FFFFFFFF != 0 {
                // supporting this will require use of an additional register
                // that is also pushed to the stack
                panic!("TODO: Support double precision floats with non-zero LSW: {}", hex_rep);
            }

            // due to limited width of immediate, we split the initialization
            // over multiple instructions:
            // TODO: use of i32 here could cause silent truncation
            let msw_upper : i32 = ((bits >> 32) & 0xF0000000) as i32;
            let msw_lower : i32 = ((bits >> 32) & 0x0FFFFFFF) as i32;
            println!("MSW upper: {:x}", msw_upper);
            println!("MSW lower: {:x}", msw_lower);
            basic_block.instructions.push(ArmIns::MoveImm {
                dst: "r7",
                imm: msw_upper,
            });
            basic_block.instructions.push(ArmIns::AddImm {
                dst: "r7",
                src: "r7",
                imm: msw_lower,
            });
            let offset = basic_block.stack_allocate_double();
            basic_block.instructions.push(ArmIns::StoreOffset {
                dst: "fp",
                src: "r7",
                offsets: vec![offset],
            });
            vec![Offset::Stack(Type::Float, offset)]
        },
        parser::AstNode::Terms(terms) => {
            let mut val_offsets = vec![];
            for term in terms {
                val_offsets.extend(compile_expr(&globalctx,basic_block, term));
            }
            val_offsets
        },
        parser::AstNode::MonadicOp {verb, expr} => {
            basic_block.instructions.push(ArmIns::Nop);
            let val_offsets = compile_expr(&globalctx, basic_block, expr);
            basic_block.instructions.push(ArmIns::Nop);
            for offset in &val_offsets {
                match offset {
                    Offset::Stack(_type, offset) =>
                        basic_block.instructions.push(ArmIns::LoadOffset {
                            dst: "r4", src: "fp", offsets: vec![offset.clone()]}),
                    Offset::Global(_type, _ident) =>
                        unimplemented!("TODO: Support loading from global.")
                };
                match verb {
                    MonadicVerb::Increment => {
                        basic_block.instructions.push(ArmIns::AddImm {
                            dst: "r4",
                            src: "r4",
                            imm: 1
                        });
                    },
                    MonadicVerb::Square => {
                        basic_block.instructions.push(ArmIns::Multiply {
                            dst: "r4",
                            src: "r4",
                            mul: "r4"
                        });
                    },
                    MonadicVerb::Negate => {
                        basic_block.instructions.push(ArmIns::MoveImm {
                            dst: "r6",
                            imm: 0
                        });
                        basic_block.instructions.push(ArmIns::Sub {
                            dst: "r4",
                            src: "r6",
                            sub: "r4"
                        });
                    }
                    _ => unimplemented!("TODO: Support monadic verb: {:?}", verb)
                }
                match offset {
                    Offset::Stack(_type, offset) =>
                        basic_block.instructions.push(ArmIns::StoreOffset {
                            src: "r4",
                            dst: "fp",
                            offsets: vec![offset.clone()]
                        }),
                    Offset::Global(_type, _ident) =>
                        unimplemented!("TODO: Support storing to global.")
                };
            }
            basic_block.instructions.push(ArmIns::Nop);
            val_offsets   // we return the same list of offsets,
                          // because we've updated them in-place
        },
        parser::AstNode::DyadicOp {verb, lhs, rhs} => {
            basic_block.instructions.push(ArmIns::Nop);
            let rhs_offsets = compile_expr(&globalctx, basic_block, rhs);
            let lhs_offsets = compile_expr(&globalctx, basic_block, lhs);
            basic_block.instructions.push(ArmIns::Nop);
            if rhs_offsets.len() != lhs_offsets.len()
                && (lhs_offsets.len() != 1 && rhs_offsets.len() != 1) {
                    panic!("Dyadic op lhs has length {}, rhs has length {}; don't know how to proceed.", lhs_offsets.len(), rhs_offsets.len())
            }

            let repeated_offset = match lhs_offsets.len() {
                1 => lhs_offsets.get(0).unwrap(),
                _ => rhs_offsets.get(0).unwrap()
            };
            let mut dest_offsets = vec![];

            for pair in lhs_offsets.iter().zip_longest(rhs_offsets.iter()) {
                let (l, r) = match pair {
                    Both(l, r) => (l, r),
                    Left(l) => (l, repeated_offset),
                    Right(r) => (repeated_offset, r)
                };
                let l_type = match l {
                    Offset::Stack(ty, i) => {
                        basic_block.instructions.push(
                            ArmIns::LoadOffset { dst: "r3", src: "fp", offsets: vec![*i] });
                        ty
                    },
                    Offset::Global(ty, ident) => {
                        basic_block.instructions.push(
                            ArmIns::Load { dst: "r3".to_string(), src: format!(".{}", ident).to_string() });
                        basic_block.instructions.push(
                            ArmIns::Load { dst: "r3".to_string(), src: "[r3]".to_string() });
                        ty
                    }
                };
                let r_type = match r {
                    Offset::Stack(ty, i) => {
                        basic_block.instructions.push(
                            ArmIns::LoadOffset { dst: "r4", src: "fp", offsets: vec![*i] });
                        ty
                    }
                    Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
                };
                let unified_type = unify_types(l_type, r_type);
                match verb {
                    DyadicVerb::Plus =>
                        basic_block.instructions.push(
                            ArmIns::Add { dst: "r4", src: "r3", add: "r4" }),
                    DyadicVerb::Times =>
                        basic_block.instructions.push(
                            ArmIns::Multiply { dst: "r4", src: "r3", mul: "r4" }),
                    DyadicVerb::Minus =>
                        basic_block.instructions.push(
                            ArmIns::Sub { dst: "r4", src: "r3", sub: "r4" }),
                    DyadicVerb::LessThan => {
                        basic_block.instructions.push(
                            ArmIns::Compare { lhs: "r3", rhs: "r4" });
                        basic_block.instructions.push(
                            ArmIns::MoveLT { dst: "r4", src: "#1" });
                        basic_block.instructions.push(
                            ArmIns::MoveGE { dst: "r4", src: "#0" });
                    },
                    DyadicVerb::Equal => {
                        basic_block.instructions.push(
                            ArmIns::Compare { lhs: "r3", rhs: "r4" });
                        basic_block.instructions.push(
                            ArmIns::MoveEQ { dst: "r4", src: "#1" });
                        basic_block.instructions.push(
                            ArmIns::MoveNE { dst: "r4", src: "#0" });
                    }
                    DyadicVerb::LargerThan => {
                        basic_block.instructions.push(
                            ArmIns::Compare { lhs: "r3", rhs: "r4" });
                        basic_block.instructions.push(
                            ArmIns::MoveGT { dst: "r4", src: "#1" });
                        basic_block.instructions.push(
                            ArmIns::MoveLE { dst: "r4", src: "#0" });
                    }
                    _ => panic!("Not ready to compile dyadic verb: {:?}", verb)
                }
                let dest_offset = basic_block._stack_allocate(4);
                basic_block.instructions.push(ArmIns::StoreOffset {
                    src: "r4", dst: "fp", offsets: vec![dest_offset] });
                dest_offsets.push(Offset::Stack(unified_type, dest_offset));
            }
            dest_offsets
        },
        parser::AstNode::Reduce {verb, expr} => {
            basic_block.instructions.push(ArmIns::Nop);
            let expr_offsets = compile_expr(&globalctx, basic_block, expr);
            basic_block.instructions.push(ArmIns::Nop);
            // Initialize the accumulator to expr's last offset value
            let accum_offset = expr_offsets.last().unwrap();
            match accum_offset {
                Offset::Stack(_type, i) =>
                    basic_block.instructions.push(ArmIns::LoadOffset {
                        dst: "r3", src: "fp", offsets: vec![*i] }),
                Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
            }
            // Accumulate from right to left.
            for offset_idx in expr_offsets[0..expr_offsets.len()-1].iter().rev() {
                match offset_idx {
                    Offset::Stack(_type, i) =>
                        basic_block.instructions.push(ArmIns::LoadOffset {
                            dst: "r4", src: "fp", offsets: vec![*i] }),
                    Offset::Global(_type, _ident) => unimplemented!("TODO: Support load from global.")
                };
                match verb {
                    DyadicVerb::Plus => {
                        basic_block.instructions.push(ArmIns::Add {
                            dst: "r3",
                            src: "r4",
                            add: "r3"
                        });
                    },
                    DyadicVerb::Minus => {
                        basic_block.instructions.push(ArmIns::Sub {
                            dst: "r3",
                            src: "r4",
                            sub: "r3"
                        });
                    },
                    DyadicVerb::Times => {
                        basic_block.instructions.push(ArmIns::Multiply {
                            dst: "r3",
                            src: "r4",
                            mul: "r3"
                        });
                    },
                    _ => unimplemented!("TODO: Support reduction of monadic verb: {:?}", verb)
                }
            }
            // Store the accumulator in expr's first offset, and return that
            // single offset here.
            match accum_offset {
                Offset::Stack(_type, i) =>
                    basic_block.instructions.push(ArmIns::StoreOffset {
                        src: "r3", dst: "fp", offsets: vec![*i] }),
                Offset::Global(_type, _ident) => unimplemented!("TODO: Support store to global.")
            };
            vec![accum_offset.clone()]
        },
        parser::AstNode::IsGlobal {ident, expr} => {
            let expr_offsets = compile_expr(&globalctx, basic_block, expr);
            if expr_offsets.len() != 1 {
                panic!("Not ready to save global '{}' comprised of multiple offsets: {:?}", ident, expr_offsets);
            }
            let offset = expr_offsets.get(0).unwrap();
            match offset {
                Offset::Stack(_type, i) =>
                    basic_block.instructions.push(ArmIns::LoadOffset {
                        dst: "r2", src: "fp", offsets: vec![*i] }),
                Offset::Global(_type, ident) => {
                    basic_block.instructions.push(ArmIns::Load {
                        dst: "r2".to_string(),
                        src: format!(".{}", ident).to_string()
                    });
                    basic_block.instructions.push(ArmIns::Load {
                        dst: "r2".to_string(),
                        src: "[r2]".to_string(),
                    });
                }
            };
            basic_block.instructions.push(ArmIns::Load {
                src: format!(".{}", ident),
                dst: "r3".to_string(),
            });
            basic_block.instructions.push(ArmIns::Store {
                src: "r2".to_string(),
                dst: "r3".to_string(),
            });
            vec![offset.clone()]
        },
        parser::AstNode::Ident(ident) => {
            if !globalctx.globals_table.contains_key(ident) {
                panic!("Program error: reference to undeclared variable: {}", ident)
            }
            let ty = globalctx.ident_type_map.get(ident).unwrap();
            vec![Offset::Global(ty.clone(), ident.clone())]
        },
        _ => panic!("Not ready to compile expression: {:?}", expr),
    }
}

fn compute_frame_size(expr: &AstNode) -> i32 {
    match expr {
        parser::AstNode::Integer(_int) => 4,
        parser::AstNode::DoublePrecisionFloat(_double) => 4,
        parser::AstNode::Terms(terms) =>
            terms.iter().map(|e| compute_frame_size(e)).sum(),
        parser::AstNode::MonadicOp {verb: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::DyadicOp {verb: _, lhs, rhs} => {
            let lhs_size = compute_frame_size(lhs);
            let rhs_size = compute_frame_size(rhs);
            // the max term is where we'll store the intermediates; ideally
            // we'd optimize this by overwriting memory addresses that we know are
            // no longer used, but this requires care b/c we can't appropriate globals.
            lhs_size + rhs_size + max(lhs_size, rhs_size)
        }
        parser::AstNode::Reduce {verb: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::IsGlobal {ident: _, expr} =>
            compute_frame_size(expr),
        parser::AstNode::Ident(_ident) => 0,
        _ => panic!("Not ready to compute frame size of expr: {:?}", expr)
    }
}

fn register_globals(expr: &AstNode,
                    registered_idents: &mut HashSet<String>,
                    ident_type_map: &mut HashMap<String, Type>) {
    match expr {
        parser::AstNode::Print(expr) =>
            register_globals(expr, registered_idents, ident_type_map),
        parser::AstNode::Integer(_int) => (),
        parser::AstNode::DoublePrecisionFloat(_double) => (),
        parser::AstNode::Terms(terms) =>
            terms.iter().for_each(|e| register_globals(e, registered_idents, ident_type_map)),
        parser::AstNode::MonadicOp {verb: _, expr} =>
            register_globals(expr, registered_idents, ident_type_map),
        parser::AstNode::DyadicOp {verb: _, lhs, rhs} => {
            register_globals(lhs, registered_idents, ident_type_map);
            register_globals(rhs, registered_idents, ident_type_map);
        }
        parser::AstNode::Reduce {verb: _, expr} =>
            register_globals(expr, registered_idents, ident_type_map),
        parser::AstNode::IsGlobal {ident, expr} => {
            registered_idents.insert(ident.clone());
            ident_type_map.insert(ident.clone(), determine_type(expr, &ident_type_map).unwrap());
            register_globals(expr, registered_idents, ident_type_map)
        },
        parser::AstNode::Ident(_ident) => (),
        _ => panic!("Not ready to register globals declared in expr: {:?}", expr)
    }
}

fn determine_type(expr: &AstNode, ident_type_map: &HashMap<String, Type>) -> Option<Type> {
    match expr {
        parser::AstNode::Integer(_int) => Some(Type::Integer),
        parser::AstNode::DoublePrecisionFloat(_double) => Some(Type::Float),
        parser::AstNode::Ident(ident) => Some(ident_type_map.get(ident).unwrap().clone()),
        parser::AstNode::DyadicOp{verb: _, lhs, rhs} =>
            Some(unify_types(
                        &determine_type(lhs, &ident_type_map).unwrap(),
                        &determine_type(rhs, &ident_type_map).unwrap())),
        _ => panic!("TODO: Unprepared to determine type of {:?}", expr)
    }
}

pub fn register_cli_options(_options: &mut Options) {}

pub fn init_from_cli_options(_matches: &Matches) -> Result<Box<dyn (::Backend)>, String> {
    Ok(Box::new(ARMBackend {}))
}
