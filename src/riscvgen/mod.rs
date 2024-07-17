use koopa::ir::{Program, Value};
use riscv::ASMBuildable;

mod riscv;

pub fn generate_asm(program: &Program, output_file: std::fs::File) -> Result<(), String> {
    let context = ASMContext {
        curr_time: 0,
        register_user: [None; 32],
        register_used_time: [0; 32],
        output_file,
    };

    program.build(&mut context)?;
    Ok(())
}

pub struct ASMContext {
    curr_time: i32,
    register_user: [Option<Value>; 32],
    register_used_time: [i32; 32], // LRU registers
    output_file: std::fs::File,
}

const REGISTER_NAMES: [&str; 32] = [
    "x0", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "fp", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5",
    "t6",
];

const REGISTER_FOR_TEMP: [usize; 15] = [5, 6, 7, 28, 29, 30, 31, 10, 11, 12, 13, 14, 15, 16, 17];
