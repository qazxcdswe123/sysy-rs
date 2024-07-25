use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type,
};

use crate::ast::*;

use super::{
    insert_instructions, new_value, DumpIR, DumpResult, ExpDumpIR, ExpDumpResult, IRContext,
    SymbolTableEntry,
};

/// Values may be none when calculating const.
fn build_binary_expression_from_results(
    lhs: ExpDumpResult,
    rhs: ExpDumpResult,
    program: &mut Program,
    context: &mut IRContext,
    op: BinaryOp,
) -> Result<ExpDumpResult, String> {
    if let (ExpDumpResult::Const(i1), ExpDumpResult::Const(i2)) = (lhs, rhs) {
        let const_res = const_propogation(op, i1, i2);
        return Ok(ExpDumpResult::Const(const_res));
    }

    let val1 = match lhs {
        ExpDumpResult::Const(c) => new_value(program, context).integer(c),
        ExpDumpResult::Value(v) => v,
    };
    let val2 = match rhs {
        ExpDumpResult::Const(c) => new_value(program, context).integer(c),
        ExpDumpResult::Value(v) => v,
    };

    let new_value = new_value(program, context).binary(op, val1, val2);
    insert_instructions(program, context, [new_value]);
    Ok(ExpDumpResult::Value(new_value))
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
    ) -> Result<DumpResult, String> {
        self.func_def.dump_ir(program, context)
    }
}

impl DumpIR for FuncDef {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
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
    ) -> Result<DumpResult, String> {
        let mut block_res = DumpResult::Ok;
        context.symbol_tables.new_table();
        for item in &self.items {
            let res = item.dump_ir(program, context)?;
            // return in block
            if let DumpResult::Abort = res {
                block_res = res;
                break;
            }
        }
        context.symbol_tables.pop_table();
        Ok(block_res)
    }
}

impl DumpIR for BlockItem {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
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
    ) -> Result<DumpResult, String> {
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
    ) -> Result<DumpResult, String> {
        let btype = &self.btype;
        for var_def in &self.var_defs {
            match &var_def.init {
                Some(rhs) => {
                    let res = rhs.dump_ir(program, context)?;
                    let rhs_value = match res {
                        ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                        ExpDumpResult::Value(v) => v,
                    };
                    let curr_func_data = program.func_mut(context.curr_func.unwrap());
                    let val = curr_func_data
                        .dfg_mut()
                        .new_value()
                        .alloc(Type::get(btype.ty.clone()));
                    curr_func_data.dfg_mut().set_value_name(
                        val,
                        Some(format!(
                            "@{}_{}",
                            var_def.ident.id,
                            context.symbol_tables.depth()
                        )),
                    );
                    let store = curr_func_data.dfg_mut().new_value().store(rhs_value, val);
                    context.symbol_tables.insert(
                        var_def.ident.id.clone(),
                        SymbolTableEntry::Variable(btype.ty.clone(), val),
                    );
                    curr_func_data
                        .layout_mut()
                        .bb_mut(context.curr_block.unwrap())
                        .insts_mut()
                        .extend([val, store]);
                }
                None => {
                    let val = new_value(program, context).alloc(Type::get(btype.ty.clone()));
                    program
                        .func_mut(context.curr_func.unwrap())
                        .dfg_mut()
                        .set_value_name(
                            val,
                            Some(format!(
                                "@{}_{}",
                                var_def.ident.id,
                                context.symbol_tables.depth()
                            )),
                        );
                    context.symbol_tables.insert(
                        var_def.ident.id.clone(),
                        SymbolTableEntry::Variable(btype.ty.clone(), val),
                    );
                    insert_instructions(program, context, [val]);
                } // VarDef::WithInitVal(ident, rhs) => {
                  // }
            }
        }
        Ok(DumpResult::Ok)
    }
}

impl ExpDumpIR for InitVal {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        let InitVal::Exp(exp) = self;
        exp.dump_ir(program, context)
    }
}

impl DumpIR for ConstDecl {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        let ty = &self.btype.ty;
        for const_def in &self.const_defs {
            let res = const_def.const_init_val.dump_ir(program, context)?;
            match res {
                ExpDumpResult::Const(c) => {
                    context.symbol_tables.insert(
                        const_def.ident.id.clone(),
                        SymbolTableEntry::Constant(ty.clone(), vec![c]),
                    );
                }
                ExpDumpResult::Value(_) => {
                    return Err(format!(
                        "Const {} is not a constant expression",
                        const_def.ident.id,
                    ));
                }
            }
        }
        Ok(DumpResult::Ok)
    }
}

impl ExpDumpIR for ConstInitVal {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        let ConstInitVal::ConstExp(exp) = self;
        exp.dump_ir(program, context)
    }
}

impl ExpDumpIR for ConstExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        self.exp.dump_ir(program, context)
    }
}

impl DumpIR for Stmt {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        match self {
            Stmt::ReturnStmt(ret_exp) => {
                let res = match ret_exp {
                    Some(exp) => Some({
                        let res = exp.dump_ir(program, context)?;
                        match res {
                            ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                            ExpDumpResult::Value(v) => v,
                        }
                    }),
                    None => None,
                };
                let ret_stmt = new_value(program, context).ret(res);
                insert_instructions(program, context, [ret_stmt]);
                Ok(DumpResult::Abort)
            }
            Stmt::AssignStmt(lval, rhs_exp) => {
                let res1 = lval.dump_ir(program, context)?;
                let lval_value = match res1 {
                    ExpDumpResult::Const(_) => {
                        return Err("LVal is should not be const".to_string());
                    }
                    ExpDumpResult::Value(v) => v,
                };
                let res2 = rhs_exp.dump_ir(program, context)?;
                let rhs_value = match res2 {
                    ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                    ExpDumpResult::Value(v) => v,
                };

                let store_inst = new_value(program, context).store(rhs_value, lval_value);
                insert_instructions(program, context, [lval_value, store_inst]);
                Ok(DumpResult::Ok)
            }
            Stmt::ExpStmt(e) => {
                if let Some(exp) = e {
                    exp.dump_ir(program, context)?;
                    Ok(DumpResult::Ok)
                } else {
                    Ok(DumpResult::Ok)
                }
            }
            Stmt::BlockStmt(b) => b.dump_ir(program, context),
        }
    }
}

impl ExpDumpIR for Exp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            Exp::LOrExp(exp) => exp.dump_ir(program, context),
        }
    }
}

impl ExpDumpIR for UnaryExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.dump_ir(program, context),
            UnaryExp::PlusUnaryExp(exp) => exp.dump_ir(program, context),
            UnaryExp::MinusUnaryExp(exp) => build_binary_expression_from_results(
                ExpDumpResult::Const(0),
                exp.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Sub,
            ),
            UnaryExp::NotUnaryExp(exp) => build_binary_expression_from_results(
                ExpDumpResult::Const(0),
                exp.dump_ir(program, context)?,
                program,
                context,
                BinaryOp::Eq,
            ),
        }
    }
}

impl ExpDumpIR for PrimaryExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            PrimaryExp::ParenExp(exp) => exp.dump_ir(program, context),
            PrimaryExp::Number(n) => n.dump_ir(program, context),
            PrimaryExp::LVal(lval) => match context.symbol_tables.get(&lval.ident.id) {
                Some(SymbolTableEntry::Constant(_, _)) => lval.dump_ir(program, context),
                Some(SymbolTableEntry::Variable(_, _)) => {
                    let res = lval.dump_ir(program, context)?;
                    match res {
                        ExpDumpResult::Const(_) => Ok(res),
                        ExpDumpResult::Value(v) => {
                            // load the value
                            let load_inst = new_value(program, context).load(v);
                            insert_instructions(program, context, [load_inst]);
                            Ok(ExpDumpResult::Value(load_inst))
                        }
                    }
                }
                None => Err(format!("Variable {} not found", lval.ident.id)),
            },
        }
    }
}

impl ExpDumpIR for LVal {
    fn dump_ir(
        &self,
        _program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match context.symbol_tables.get(&self.ident.id) {
            Some(SymbolTableEntry::Variable(_tk, val)) => Ok(ExpDumpResult::Value(*val)),
            Some(SymbolTableEntry::Constant(_tk, val)) => Ok(ExpDumpResult::Const(val[0])),
            None => Err(format!("Variable {} not found", self.ident.id)),
        }
    }
}

impl ExpDumpIR for AddExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
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

impl ExpDumpIR for MulExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
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

impl ExpDumpIR for LOrExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            LOrExp::LAndExp(l) => l.dump_ir(program, context),
            LOrExp::BinaryLOrExp(lhs, rhs) => {
                let res1 = build_binary_expression_from_results(
                    lhs.dump_ir(program, context)?,
                    ExpDumpResult::Const(0),
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let res2 = build_binary_expression_from_results(
                    ExpDumpResult::Const(0),
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

impl ExpDumpIR for LAndExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            LAndExp::EqExp(e) => e.dump_ir(program, context),
            LAndExp::BinaryLAndExp(lhs, rhs) => {
                let res1 = build_binary_expression_from_results(
                    lhs.dump_ir(program, context)?,
                    ExpDumpResult::Const(0),
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let res2 = build_binary_expression_from_results(
                    ExpDumpResult::Const(0),
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

impl ExpDumpIR for EqExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
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

impl ExpDumpIR for RelExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
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

impl ExpDumpIR for Number {
    fn dump_ir(
        &self,
        _program: &mut Program,
        _context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            Number::IntConst(i) => Ok(ExpDumpResult::Const(*i)),
        }
    }
}
