use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type,
};

use crate::ast::*;

use super::{insert_instructions, new_value, ConstOrValue, DumpIR, IRContext, SymbolTableEntry};

// fn build_binary_expression(
//     first_exp: &dyn DumpIR,
//     second_exp: &dyn DumpIR,
//     program: &mut Program,
//     context: &mut IRContext,
//     op: BinaryOp,
// ) -> Result<ConstOrValue, String> {
//     let mut tmp1 = 0;
//     first_exp.dump_ir(program, context)?;
//     let lhs = context.curr_value;
//     if let Some((tmp, _)) = context.tmp_const {
//         tmp1 = tmp;
//     }
//     second_exp.dump_ir(program, context)?;
//     let rhs = context.curr_value;
//     if let Some((tmp2, _)) = context.tmp_const {
//         context.tmp_const = Some((tmp1, tmp2));
//     }

//     build_binary_expression_from_results(lhs, rhs, program, context, op)?;

//     Ok(())
// }

/// Values may be none when calculating const.
fn build_binary_expression_from_results(
    lhs: ConstOrValue,
    rhs: ConstOrValue,
    program: &mut Program,
    context: &mut IRContext,
    op: BinaryOp,
) -> Result<ConstOrValue, String> {
    if let (ConstOrValue::Const(i1), ConstOrValue::Const(i2)) = (lhs, rhs) {
        let const_res = const_propogation(op, i1, i2);
        return Ok(ConstOrValue::Const(const_res));
    }

    let val1 = match lhs {
        ConstOrValue::Const(c) => new_value(program, context).integer(c),
        ConstOrValue::Value(v) => v,
    };
    let val2 = match rhs {
        ConstOrValue::Const(c) => new_value(program, context).integer(c),
        ConstOrValue::Value(v) => v,
    };

    let new_value = new_value(program, context).binary(op, val1, val2);
    insert_instructions(program, context, [new_value]);
    Ok(ConstOrValue::Value(new_value))
}

fn const_propogation(op: BinaryOp, int1: i32, int2: i32) -> i32 {
    match op {
        BinaryOp::NotEq => (int1 != int2) as i32,
        BinaryOp::Eq => (int1 == int2) as i32,
        BinaryOp::Gt => (int1 > int2) as i32,
        BinaryOp::Lt => (int1 < int2) as i32,
        BinaryOp::Ge => (int1 >= int2) as i32,
        BinaryOp::Le => (int1 <= int2) as i32,
        BinaryOp::Add => int1 + int2,
        BinaryOp::Sub => int1 - int2,
        BinaryOp::Mul => int1 * int2,
        BinaryOp::Div => int1 / int2,
        BinaryOp::Mod => int1 % int2,
        BinaryOp::And => int1 & int2,
        BinaryOp::Or => int1 | int2,
        BinaryOp::Xor => int1 ^ int2,
        BinaryOp::Shl => todo!(),
        BinaryOp::Shr => todo!(),
        BinaryOp::Sar => todo!(),
    }
}

impl DumpIR for CompUnit {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        self.func_def.dump_ir(program, context)
    }
}

impl DumpIR for FuncDef {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        let ret_ty = self.func_type.ty.clone();
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident.id),
            vec![],
            Type::get(ret_ty),
        ));
        let func_data = program.func_mut(func);
        let new_block = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        func_data.layout_mut().bbs_mut().extend([new_block]);
        context.curr_block = Some(new_block);
        context.curr_func = Some(func);
        self.block.dump_ir(program, context)
    }
}

impl DumpIR for Block {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        for item in &self.items {
            item.dump_ir(program, context)?;
            // ignore everything after return
            if let Some(SymbolTableEntry::Variable(_, _)) = context.symbol_table.get("return") {
                break;
            }
        }
        Ok(ConstOrValue::Const(0))
    }
}

impl DumpIR for BlockItem {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            BlockItem::Decl(decl) => decl.dump_ir(program, context),
            BlockItem::Stmt(stmt) => stmt.dump_ir(program, context),
        }
    }
}

impl DumpIR for Decl {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.dump_ir(program, context),
            Decl::VarDecl(var_decl) => var_decl.dump_ir(program, context),
        }
    }
}

impl DumpIR for VarDecl {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        let btype = &self.btype;
        for var_def in &self.var_defs {
            match var_def {
                VarDef::WithoutInitVal(ident) => {
                    let curr_func = program.func_mut(context.curr_func.unwrap());

                    let val = curr_func
                        .dfg_mut()
                        .new_value()
                        .alloc(Type::get(btype.ty.clone()));
                    curr_func
                        .dfg_mut()
                        .set_value_name(val, Some(format!("@{}", ident.id)));
                    context.symbol_table.insert(
                        ident.id.clone(),
                        SymbolTableEntry::Variable(btype.ty.clone(), val),
                    );

                    curr_func
                        .layout_mut()
                        .bb_mut(context.curr_block.unwrap())
                        .insts_mut()
                        .extend([val])
                }
                VarDef::WithInitVal(ident, rhs) => {
                    let res = rhs.dump_ir(program, context)?;
                    let rhs_value = match res {
                        ConstOrValue::Const(c) => new_value(program, context).integer(c),
                        ConstOrValue::Value(v) => v,
                    };
                    let curr_func_data = program.func_mut(context.curr_func.unwrap());
                    let val = curr_func_data
                        .dfg_mut()
                        .new_value()
                        .alloc(Type::get(btype.ty.clone()));
                    curr_func_data
                        .dfg_mut()
                        .set_value_name(val, Some(format!("@{}", ident.id)));
                    let store = curr_func_data.dfg_mut().new_value().store(rhs_value, val);
                    context.symbol_table.insert(
                        ident.id.clone(),
                        SymbolTableEntry::Variable(btype.ty.clone(), val),
                    );
                    curr_func_data
                        .layout_mut()
                        .bb_mut(context.curr_block.unwrap())
                        .insts_mut()
                        .extend([val, store]);
                }
            }
        }
        Ok(ConstOrValue::Const(0))
    }
}

impl DumpIR for InitVal {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        let InitVal::Exp(exp) = self;
        exp.dump_ir(program, context)
    }
}

impl DumpIR for ConstDecl {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        let ty = &self.btype.ty;
        for const_def in &self.const_defs {
            let res = const_def.const_init_val.dump_ir(program, context)?;
            match res {
                ConstOrValue::Const(c) => {
                    context.symbol_table.insert(
                        const_def.ident.id.clone(),
                        SymbolTableEntry::Constant(ty.clone(), vec![c]),
                    );
                }
                ConstOrValue::Value(_) => {
                    return Err(format!(
                        "Const {} is not a constant expression",
                        const_def.ident.id,
                    ));
                }
            }
        }
        Ok(ConstOrValue::Const(0))
    }
}

impl DumpIR for ConstInitVal {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        let ConstInitVal::ConstExp(exp) = self;
        exp.dump_ir(program, context)
    }
}

impl DumpIR for ConstExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        self.exp.dump_ir(program, context)
    }
}

impl DumpIR for Stmt {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            Stmt::ReturnStmt(ret_exp) => {
                let res = ret_exp.dump_ir(program, context)?;
                let val = match res {
                    ConstOrValue::Const(c) => new_value(program, context).integer(c),
                    ConstOrValue::Value(v) => v,
                };
                let ret_stmt = new_value(program, context).ret(Some(val));
                insert_instructions(program, context, [ret_stmt]);
                Ok(ConstOrValue::Const(0))
            }
            Stmt::AssignStmt(lval, rhs_exp) => {
                let res1 = lval.dump_ir(program, context)?;
                let lval_value = match res1 {
                    ConstOrValue::Const(_) => {
                        return Err("LVal is should not be const".to_string());
                    }
                    ConstOrValue::Value(v) => v,
                };
                let res2 = rhs_exp.dump_ir(program, context)?;
                let rhs_value = match res2 {
                    ConstOrValue::Const(c) => new_value(program, context).integer(c),
                    ConstOrValue::Value(v) => v,
                };

                let store_inst = new_value(program, context).store(rhs_value, lval_value);
                insert_instructions(program, context, [lval_value, store_inst]);
                Ok(ConstOrValue::Const(0))
            }
        }
    }
}

impl DumpIR for Exp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            Exp::LOrExp(exp) => exp.dump_ir(program, context),
        }
    }
}

impl DumpIR for UnaryExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.dump_ir(program, context),
            UnaryExp::PlusUnaryExp(exp) => exp.dump_ir(program, context),
            UnaryExp::MinusUnaryExp(exp) => build_binary_expression_from_results(
                ConstOrValue::Const(0),
                exp.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Sub,
            ),
            UnaryExp::NotUnaryExp(exp) => build_binary_expression_from_results(
                ConstOrValue::Const(0),
                exp.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Eq,
            ),
        }
    }
}

impl DumpIR for PrimaryExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            PrimaryExp::ParenExp(exp) => exp.dump_ir(program, context),
            PrimaryExp::Number(n) => n.dump_ir(program, context),
            PrimaryExp::LVal(lval) => match context.symbol_table.get(&lval.ident.id) {
                Some(SymbolTableEntry::Constant(_, _)) => lval.dump_ir(program, context),
                Some(SymbolTableEntry::Variable(_, _)) => {
                    let res = lval.dump_ir(program, context)?;
                    match res {
                        ConstOrValue::Const(_) => Ok(res),
                        ConstOrValue::Value(v) => {
                            // load the value
                            let load_inst = new_value(program, context).load(v);
                            insert_instructions(program, context, [load_inst]);
                            Ok(ConstOrValue::Value(load_inst))
                        }
                    }
                }
                None => Err(format!("Variable {} not found", lval.ident.id)),
            },
        }
    }
}

impl DumpIR for LVal {
    fn dump_ir(
        &self,
        _program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match context.symbol_table.get(&self.ident.id) {
            Some(SymbolTableEntry::Variable(_tk, val)) => Ok(ConstOrValue::Value(*val)),
            Some(SymbolTableEntry::Constant(_tk, val)) => Ok(ConstOrValue::Const(val[0])),
            None => Err(format!("Variable {} not found", self.ident.id)),
        }
    }
}

impl DumpIR for AddExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            AddExp::MulExp(m) => m.dump_ir(program, context),
            AddExp::BinaryAddExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Add,
            ),
            AddExp::BinarySubExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Sub,
            ),
        }
    }
}

impl DumpIR for MulExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            MulExp::UnaryExp(u) => u.dump_ir(program, context),
            MulExp::BinaryMulExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Mul,
            ),
            MulExp::BinaryDivExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Div,
            ),
            MulExp::BinaryModExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Mod,
            ),
        }
    }
}

impl DumpIR for LOrExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            LOrExp::LAndExp(l) => l.dump_ir(program, context),
            LOrExp::BinaryLOrExp(lhs, rhs) => {
                let res1 = build_binary_expression_from_results(
                    lhs.dump_ir(program, context)?,
                    ConstOrValue::Const(0),
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let res2 = build_binary_expression_from_results(
                    ConstOrValue::Const(0),
                    rhs.dump_ir(program, context)?,
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;

                build_binary_expression_from_results(res1, res2, program, context, BinaryOp::Or)
            }
        }
    }
}

impl DumpIR for LAndExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            LAndExp::EqExp(e) => e.dump_ir(program, context),
            LAndExp::BinaryLAndExp(lhs, rhs) => {
                let res1 = build_binary_expression_from_results(
                    lhs.dump_ir(program, context)?,
                    ConstOrValue::Const(0),
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let res2 = build_binary_expression_from_results(
                    ConstOrValue::Const(0),
                    rhs.dump_ir(program, context)?,
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;

                build_binary_expression_from_results(res1, res2, program, context, BinaryOp::And)
            }
        }
    }
}

impl DumpIR for EqExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            EqExp::RelExp(e) => e.dump_ir(program, context),
            EqExp::BinaryEqExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Eq,
            ),
            EqExp::BinaryNeExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::NotEq,
            ),
        }
    }
}

impl DumpIR for RelExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            RelExp::AddExp(exp) => exp.dump_ir(program, context),
            RelExp::BinaryLtExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Lt,
            ),
            RelExp::BinaryGtExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Gt,
            ),
            RelExp::BinaryLeExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Le,
            ),
            RelExp::BinaryGeExp(lhs, rhs) => build_binary_expression_from_results(
                lhs.dump_ir(program, context)?,
                rhs.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Ge,
            ),
        }
    }
}

impl DumpIR for Number {
    fn dump_ir(
        &self,
        _program: &mut Program,
        _context: &mut IRContext,
    ) -> Result<ConstOrValue, String> {
        match self {
            Number::IntConst(i) => Ok(ConstOrValue::Const(*i)),
        }
    }
}
