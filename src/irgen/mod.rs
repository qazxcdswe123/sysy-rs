use std::collections::HashMap;

use koopa::ir::{
    builder::{BasicBlockBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder},
    entities::{BasicBlock, Function, ValueData},
    Program, Type, TypeKind, Value,
};

use crate::ast::{BType, CompUnit, Exp, InitVal};

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
        function_table: HashMap::new(),
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

pub enum InitValDumpResult {
    Const(i32),
    Value(Value),
    Aggregate(Value),
}

pub enum LValDumpResult {
    Const(i32),
    Temp(Value),
    Addr(Value),
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
    function_table: HashMap<String, Function>,
    bb: u64,
}

pub struct SymbolTables {
    symbol_tables: Vec<HashMap<String, SymbolTableEntry>>,
}

pub enum SymbolTableEntry {
    Variable(Value),
    Constant(i32),
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

fn get_valuedata(program: &mut Program, context: &mut IRContext, value: Value) -> ValueData {
    if value.is_global() {
        program.borrow_value(value).clone()
    } else {
        program
            .func(context.curr_func.unwrap())
            .dfg()
            .value(value)
            .clone()
    }
}

fn build_shape(
    program: &mut Program,
    context: &mut IRContext,
    shape: &Vec<Exp>,
) -> Result<Vec<usize>, String> {
    let mut values = vec![];
    for exp in shape {
        match exp.dump_ir(program, context)? {
            ExpDumpResult::Const(c) => values.push(c as usize),
            ExpDumpResult::Value(v) => {
                return Err(format!(
                    "Expected constant expression, found value {:?}",
                    get_valuedata(program, context, v)
                ))
            }
        }
    }
    Ok(values)
}

fn get_arr_type(btype: &BType, shape: &[usize]) -> TypeKind {
    if shape.is_empty() {
        btype.ty.clone()
    } else {
        let inner_type = get_arr_type(btype, &shape[1..]);
        TypeKind::Array(Type::get(inner_type), shape[0])
    }
}

// fn build_new_aggregate(
//     program: &mut Program,
//     context: &mut IRContext,
//     shape: &[usize],
//     children: &[Box<InitVal>],
//     is_global: bool,
// ) -> Result<(Value, usize), String> {
//     let mut elems = Vec::new();
//     let mut curr_child_idx = 0;

//     for _ in 0..shape[0] {
//         let next_child = children.get(curr_child_idx);
//         let value = if shape.len() == 1 {
//             match next_child {
//                 Some(InitVal::Exp(exp)) => {
//                     curr_child_idx += 1;
//                     match exp.dump_ir(program, context)? {
//                         ExpDumpResult::Const(c) => {
//                             if is_global {
//                                 program.new_value().integer(c)
//                             } else {
//                                 new_value(program, context).integer(c)
//                             }
//                         }
//                         ExpDumpResult::Value(_) => {
//                             return Err("Expected constant expression".to_string());
//                         }
//                     }
//                 }
//                 Some(InitVal::Aggregate(_)) => {
//                     return Err("Nested aggregate initialization is not supported".to_string());
//                 }
//                 None => {
//                     if is_global {
//                         program.new_value().integer(0)
//                     } else {
//                         new_value(program, context).integer(0)
//                     }
//                 }
//             }
//         } else {
//             match next_child {
//                 Some(InitVal::Exp(_)) => {}
//                 Some(InitVal::Aggregate(_)) => {
//                     let res = children[curr_child_idx].dump_ir(program, context, &shape[1..])?;
//                     curr_child_idx += 1;
//                     if let InitValDumpResult::Aggregate(a) = res {
//                         a
//                     } else {
//                         unreachable!()
//                     }
//                 }
//                 None => {
//                     let new_shape = &children[curr_child_idx..];
//                     let (val, consumed) =
//                         build_new_aggregate(program, context, &shape[1..], new_shape, is_global)?;
//                     curr_child_idx += consumed;
//                     val
//                 }
//             }
//         };
//         elems.push(value);
//     }

//     let aggregate_value = if is_global {
//         program.new_value().aggregate(elems)
//     } else {
//         new_value(program, context).aggregate(elems)
//     };
//     Ok((aggregate_value, curr_child_idx))
// }

fn build_new_aggregate(
    program: &mut Program,
    context: &mut IRContext,
    shape: &[usize],
    children: &[Box<InitVal>],
    is_global: bool,
) -> Result<(Value, usize), String> {
    let mut elems = vec![];
    let mut curr_child_idx = 0;

    if shape.len() == 1 {
        for _ in 0..shape[0] {
            // 1d array, contains only exps
            let next_child = if curr_child_idx < children.len() {
                Some(&*children[curr_child_idx])
            } else {
                None
            };

            let exp_dump_res = match next_child {
                Some(InitVal::Exp(exp)) => exp.dump_ir(program, context)?,
                Some(InitVal::Aggregate(_)) => {
                    return Err("Nested aggregate initialization is not supported".to_string())
                }
                None => ExpDumpResult::Const(0),
            };
            curr_child_idx += 1;

            let value = match exp_dump_res {
                ExpDumpResult::Const(c) => {
                    if is_global {
                        program.new_value().integer(c)
                    } else {
                        new_value(program, context).integer(c)
                    }
                }
                ExpDumpResult::Value(_) => {
                    return Err("Expected constant expression".to_string());
                }
            };
            elems.push(value);
        }
    } else {
        for _ in 0..shape[0] {
            let next_child = if curr_child_idx < children.len() {
                Some(&*children[curr_child_idx])
            } else {
                None
            };

            match next_child {
                Some(InitVal::Exp(_)) => {}
                Some(InitVal::Aggregate(_)) => {
                    let res = children[curr_child_idx].dump_ir(program, context, &shape[1..])?;
                    match res {
                        InitValDumpResult::Aggregate(a) => elems.push(a),
                        _ => {
                            unreachable!()
                        }
                    }
                    curr_child_idx += 1;
                    continue;
                }
                None => {}
            }

            let new_shape = if curr_child_idx < children.len() {
                &children[curr_child_idx..]
            } else {
                &[]
            };
            let (value, consumed) =
                build_new_aggregate(program, context, &shape[1..], new_shape, is_global)?;
            curr_child_idx += consumed;
            elems.push(value);
        }
    }
    if is_global {
        Ok((program.new_value().aggregate(elems), curr_child_idx))
    } else {
        Ok((new_value(program, context).aggregate(elems), curr_child_idx))
    }
}

fn aggregate_to_store(program: &mut Program, context: &mut IRContext, aggr: Value, dest: Value) {
    if aggr.is_global() {
        unreachable!()
    }

    let valuedata = get_valuedata(program, context, aggr);
    match valuedata.kind() {
        koopa::ir::ValueKind::Aggregate(aggr) => {
            for i in 0..aggr.elems().len() {
                let index = new_value(program, context).integer(i as i32);
                let child = aggr.elems()[i];
                let child_valuedata = get_valuedata(program, context, child);
                let child_ptr = new_value(program, context).get_elem_ptr(dest, index);
                insert_instructions(program, context, [child_ptr]);
                match child_valuedata.kind() {
                    koopa::ir::ValueKind::Aggregate(_) => {
                        aggregate_to_store(program, context, child, child_ptr)
                    }
                    _ => {
                        let store_inst = new_value(program, context).store(child, child_ptr);
                        insert_instructions(program, context, [store_inst]);
                    }
                }
            }
        }
        _ => unreachable!(),
    }
}
