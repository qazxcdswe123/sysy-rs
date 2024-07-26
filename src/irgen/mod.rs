use std::collections::HashMap;

use koopa::ir::{
    builder::{BasicBlockBuilder, LocalBuilder},
    entities::{BasicBlock, Function},
    Program, TypeKind, Value,
};

use crate::ast::CompUnit;

mod irgen;

pub trait ExpDumpIR {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String>;
}

pub fn generate_ir(comp_unit: &CompUnit) -> Result<Program, String> {
    let mut program = Program::new();
    let mut context = IRContext {
        curr_block: None,
        curr_func: None,
        symbol_tables: SymbolTables {
            symbol_tables: vec![],
        },
        break_blocks: vec![],
        continue_blocks: vec![],
        bb: 0,
    };

    comp_unit.dump_ir(&mut program, &mut context)?;
    Ok(program)
}

/// IR building result. If the expression is a constant expression, returns the i32 result.
/// Otherwise, returns the Koopa IR Value.
#[derive(Clone, Copy)]
pub enum ExpDumpResult {
    Const(i32),
    Value(Value),
}

pub enum DumpResult {
    Ok,
    Abort,
}

pub trait DumpIR {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext)
        -> Result<DumpResult, String>;
}

pub struct IRContext {
    curr_block: Option<BasicBlock>,
    curr_func: Option<Function>,
    symbol_tables: SymbolTables,
    break_blocks: Vec<BasicBlock>,
    continue_blocks: Vec<BasicBlock>,
    bb: u64,
}

pub struct SymbolTables {
    symbol_tables: Vec<HashMap<String, SymbolTableEntry>>,
}

pub enum SymbolTableEntry {
    Variable(TypeKind, Value),
    Constant(TypeKind, Vec<i32>),
}

impl SymbolTables {
    fn get(&self, name: &String) -> Option<&SymbolTableEntry> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(entry) = table.get(name) {
                return Some(entry);
            }
        }
        None
    }

    fn insert(&mut self, name: String, entry: SymbolTableEntry) {
        self.symbol_tables.last_mut().unwrap().insert(name, entry);
    }

    fn new_table(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn pop_table(&mut self) {
        self.symbol_tables.pop();
    }

    fn depth(&self) -> usize {
        self.symbol_tables.len() - 1
    }
}

fn new_value<'a>(program: &'a mut Program, context: &'a mut IRContext) -> LocalBuilder<'a> {
    program
        .func_mut(context.curr_func.unwrap())
        .dfg_mut()
        .new_value()
}

fn insert_instructions<T>(program: &mut Program, context: &mut IRContext, instructions: T)
where
    T: IntoIterator<Item = Value>,
{
    program
        .func_mut(context.curr_func.unwrap())
        .layout_mut()
        .bb_mut(context.curr_block.unwrap())
        .insts_mut()
        .extend(instructions);
}

fn insert_basic_blocks<T>(program: &mut Program, context: &mut IRContext, basic_blocks: T)
where
    T: IntoIterator<Item = BasicBlock>,
{
    program
        .func_mut(context.curr_func.unwrap())
        .layout_mut()
        .bbs_mut()
        .extend(basic_blocks);
}

fn new_block(program: &mut Program, context: &mut IRContext, name: &str) -> BasicBlock {
    let blk = program
        .func_mut(context.curr_func.unwrap())
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%bb{}_{}", context.bb, name)));
    context.bb += 1;
    blk
}
