use irgen::DumpIR;
use koopa::ir::{
    entities::{BasicBlock, Function},
    Program, Value,
};

use crate::ast::CompUnit;

mod irgen;

pub fn generate_ir(comp_unit: &CompUnit) -> Result<Program, String> {
    let mut program = Program::new();
    let mut context = IRContext {
        curr_block: None,
        curr_func: None,
        curr_value: None,
    };

    comp_unit.dump_ir(&mut program, &mut context)?;
    Ok(program)
}

pub struct IRContext {
    curr_block: Option<BasicBlock>,
    curr_func: Option<Function>,
    curr_value: Option<Value>,
}
