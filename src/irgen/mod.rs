use std::collections::HashMap;

use irgen::DumpIR;
use koopa::ir::{
    builder::LocalBuilder,
    entities::{BasicBlock, Function},
    Program, TypeKind, Value,
};

use crate::ast::CompUnit;

mod irgen;

pub fn generate_ir(comp_unit: &CompUnit) -> Result<Program, String> {
    let mut program = Program::new();
    let mut context = IRContext {
        curr_block: None,
        curr_func: None,
        curr_value: None,
        symbol_table: HashMap::new(),
        tmp_const: None,
    };

    comp_unit.dump_ir(&mut program, &mut context)?;
    Ok(program)
}

pub struct IRContext {
    curr_block: Option<BasicBlock>,
    curr_func: Option<Function>,
    curr_value: Option<Value>,
    symbol_table: HashMap<String, SymbolTableEntry>,
    tmp_const: Option<(i32, i32)>,
}

pub enum SymbolTableEntry {
    Variable(TypeKind, Value),
    Constant(TypeKind, Vec<i32>),
}

impl std::fmt::Debug for SymbolTableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolTableEntry::Variable(tk, v) => write!(f, "Variable {}: {:?}", tk, v),
            SymbolTableEntry::Constant(tk, v) => write!(f, "Constant {}: {:?}", tk, v),
        }
    }
}

fn new_value<'a>(program: &'a mut Program, context: &'a mut IRContext) -> LocalBuilder<'a> {
    program
        .func_mut(context.curr_func.unwrap())
        .dfg_mut()
        .new_value()
}
