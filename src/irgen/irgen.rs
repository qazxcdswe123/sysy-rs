use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type,
};

use crate::ast::*;

use super::{IRContext, SymbolTableEntry};

fn build_binary_expression(
    first_exp: &dyn DumpIR,
    second_exp: &dyn DumpIR,
    program: &mut Program,
    context: &mut IRContext,
    op: BinaryOp,
) -> Result<(), String> {
    let mut tmp1 = 0;
    first_exp.dump_ir(program, context)?;
    let lhs = context.curr_value;
    if let Some((tmp, _)) = context.tmp_const {
        tmp1 = tmp;
    }
    second_exp.dump_ir(program, context)?;
    let rhs = context.curr_value;
    if let Some((tmp2, _)) = context.tmp_const {
        context.tmp_const = Some((tmp1, tmp2));
    }

    build_binary_expression_from_values(program, context, op, lhs, rhs)?;

    Ok(())
}

/// Values may be none when calculating const.
fn build_binary_expression_from_values(
    program: &mut Program,
    context: &mut IRContext,
    op: BinaryOp,
    lhs: Option<koopa::ir::Value>,
    rhs: Option<koopa::ir::Value>,
) -> Result<(), String> {
    if let Some((tmp1, tmp2)) = context.tmp_const {
        const_propogation(op, context, tmp1, tmp2);
        return Ok(());
    }
    let curr_func_data = program.func_mut(context.curr_func.unwrap());
    let new_value = curr_func_data
        .dfg_mut()
        .new_value()
        .binary(op, lhs.unwrap(), rhs.unwrap());
    curr_func_data
        .layout_mut()
        .bb_mut(context.curr_block.unwrap())
        .insts_mut()
        .extend([new_value]);
    context.curr_value = Some(new_value);
    Ok(())
}

fn const_propogation(op: BinaryOp, context: &mut IRContext, tmp1: i32, tmp2: i32) {
    match op {
        BinaryOp::NotEq => context.tmp_const = Some(((tmp1 != tmp2) as i32, 0)),
        BinaryOp::Eq => context.tmp_const = Some(((tmp1 == tmp2) as i32, 0)),
        BinaryOp::Gt => context.tmp_const = Some(((tmp1 > tmp2) as i32, 0)),
        BinaryOp::Lt => context.tmp_const = Some(((tmp1 < tmp2) as i32, 0)),
        BinaryOp::Ge => context.tmp_const = Some(((tmp1 >= tmp2) as i32, 0)),
        BinaryOp::Le => context.tmp_const = Some(((tmp1 <= tmp2) as i32, 0)),
        BinaryOp::Add => context.tmp_const = Some((tmp1 + tmp2, 0)),
        BinaryOp::Sub => context.tmp_const = Some((tmp1 - tmp2, 0)),
        BinaryOp::Mul => context.tmp_const = Some((tmp1 * tmp2, 0)),
        BinaryOp::Div => context.tmp_const = Some((tmp1 / tmp2, 0)),
        BinaryOp::Mod => context.tmp_const = Some((tmp1 % tmp2, 0)),
        BinaryOp::And => context.tmp_const = Some((tmp1 & tmp2, 0)),
        BinaryOp::Or => context.tmp_const = Some((tmp1 | tmp2, 0)),
        BinaryOp::Xor => context.tmp_const = Some((tmp1 ^ tmp2, 0)),
        BinaryOp::Shl => context.tmp_const = Some((tmp1 << tmp2, 0)),
        BinaryOp::Sar => context.tmp_const = Some((tmp1 >> tmp2, 0)),
        BinaryOp::Shr => unreachable!(),
    }
}

pub trait DumpIR {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String>;
}

impl DumpIR for CompUnit {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        self.func_def.dump_ir(program, context)
    }
}

impl DumpIR for FuncDef {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        let ret_ty = self.func_type.ty.clone();
        let func = program.new_func(FunctionData::with_param_names(
            "@".to_string() + self.ident.id.as_str(),
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
        self.block.dump_ir(program, context)?;
        Ok(())
    }
}

impl DumpIR for Block {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        for ele in &self.items {
            ele.dump_ir(program, context)?;
        }
        Ok(())
    }
}

impl DumpIR for BlockItem {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            BlockItem::Decl(decl) => decl.dump_ir(program, context)?,
            BlockItem::Stmt(stmt) => stmt.dump_ir(program, context)?,
        }
        context.curr_value = None;
        Ok(())
    }
}

impl DumpIR for Decl {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.dump_ir(program, context),
            Decl::VarDecl(var_decl) => var_decl.dump_ir(program, context),
        }
    }
}

impl DumpIR for VarDecl {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        let btype = &self.btype;
        for var_def in &self.var_defs {
            match var_def {
                VarDef::WithoutInitVal(ident) => {
                    let curr_func = program.func_mut(context.curr_func.unwrap());

                    let val = curr_func
                        .dfg_mut()
                        .new_value()
                        .alloc(Type::get(btype.ty.clone()));
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
                    rhs.dump_ir(program, context)?;
                    let rhs_value = context.curr_value.unwrap();
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
        Ok(())
    }
}

impl DumpIR for InitVal {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        let InitVal::Exp(exp) = self;
        exp.dump_ir(program, context)
    }
}

impl DumpIR for ConstDecl {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        let ty = &self.btype.ty;
        for const_def in &self.const_defs {
            context.tmp_const = Some((0, 0));
            const_def.const_init_val.dump_ir(program, context)?;
            let (c, _) = context.tmp_const.unwrap();
            context.tmp_const = None;
            context.symbol_table.insert(
                const_def.ident.id.clone(),
                SymbolTableEntry::Constant(ty.clone(), vec![c]),
            );
        }
        Ok(())
    }
}

impl DumpIR for ConstInitVal {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        let ConstInitVal::ConstExp(exp) = self;
        exp.dump_ir(program, context)
    }
}

impl DumpIR for ConstExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        self.exp.dump_ir(program, context)
    }
}

impl DumpIR for Stmt {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            Stmt::ReturnStmt(ret_exp) => {
                ret_exp.dump_ir(program, context)?;
                let curr_func_data = program.func_mut(context.curr_func.unwrap());
                let return_stmt = curr_func_data.dfg_mut().new_value().ret(context.curr_value);

                curr_func_data
                    .layout_mut()
                    .bb_mut(context.curr_block.unwrap())
                    .insts_mut()
                    .extend([return_stmt]);

                Ok(())
            }
            Stmt::AssignStmt(lval, rhs_exp) => {
                lval.dump_ir(program, context)?;
                let lval_value = context.curr_value.unwrap();
                rhs_exp.dump_ir(program, context)?;
                let rhs_value = context.curr_value.unwrap();

                let curr_func_data = program.func_mut(context.curr_func.unwrap());
                let assign_stmt = curr_func_data
                    .dfg_mut()
                    .new_value()
                    .store(rhs_value, lval_value);

                curr_func_data
                    .layout_mut()
                    .bb_mut(context.curr_block.unwrap())
                    .insts_mut()
                    .extend([assign_stmt]);

                Ok(())
            }
        }
    }
}

impl DumpIR for Exp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            Exp::LOrExp(exp) => exp.dump_ir(program, context),
        }
    }
}

impl DumpIR for UnaryExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            UnaryExp::PrimaryExp(exp) => exp.dump_ir(program, context),
            UnaryExp::PlusUnaryExp(exp) => exp.dump_ir(program, context),
            UnaryExp::MinusUnaryExp(exp) => build_binary_expression(
                &Number::IntConst(0),
                &**exp,
                program,
                context,
                BinaryOp::Sub,
            ),
            UnaryExp::NotUnaryExp(exp) => build_binary_expression(
                &Number::IntConst(0),
                &**exp,
                program,
                context,
                BinaryOp::Eq,
            ),
        }
    }
}

impl DumpIR for PrimaryExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            PrimaryExp::ParenExp(exp) => exp.dump_ir(program, context),
            PrimaryExp::Number(n) => n.dump_ir(program, context),
            PrimaryExp::LVal(lval) => match context.symbol_table.get(&lval.ident.id) {
                Some(SymbolTableEntry::Constant(_, _)) => lval.dump_ir(program, context),
                Some(SymbolTableEntry::Variable(_, _)) => {
                    lval.dump_ir(program, context)?;
                    let val = context.curr_value.unwrap();
                    let curr_func_data = program.func_mut(context.curr_func.unwrap());
                    let load = curr_func_data.dfg_mut().new_value().load(val);
                    context.curr_value = Some(load);
                    curr_func_data
                        .layout_mut()
                        .bb_mut(context.curr_block.unwrap())
                        .insts_mut()
                        .extend([load]);
                    Ok(())
                }
                None => Err(format!("Variable {} not found", lval.ident.id)),
            },
        }
    }
}

impl DumpIR for LVal {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match context.symbol_table.get(&self.ident.id) {
            Some(SymbolTableEntry::Variable(_tk, val)) => {
                if let Some(_) = context.tmp_const {
                    return Err("Constant not finished yet".to_string());
                }
                context.curr_value = Some(*val);
                Ok(())
            }
            Some(SymbolTableEntry::Constant(_tk, val)) => {
                if let Some(_) = context.tmp_const {
                    context.tmp_const = Some((val[0], 0));
                    return Ok(());
                }
                context.curr_value = Some(
                    program
                        .func_mut(context.curr_func.unwrap())
                        .dfg_mut()
                        .new_value()
                        .integer(val[0]),
                );
                Ok(())
            }
            None => Err(format!("Variable {} not found", self.ident.id)),
        }
    }
}

impl DumpIR for AddExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            AddExp::MulExp(m) => m.dump_ir(program, context),
            AddExp::BinaryAddExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Add)
            }
            AddExp::BinarySubExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Sub)
            }
        }
    }
}

impl DumpIR for MulExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            MulExp::UnaryExp(u) => u.dump_ir(program, context),
            MulExp::BinaryMulExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Mul)
            }
            MulExp::BinaryDivExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Div)
            }
            MulExp::BinaryModExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Mod)
            }
        }
    }
}

impl DumpIR for LOrExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            LOrExp::LAndExp(l) => l.dump_ir(program, context),
            LOrExp::BinaryLOrExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::NotEq)?;
                let mut tmp1 = 0;
                if let Some((tmp, _)) = context.tmp_const {
                    tmp1 = tmp;
                }
                let left_bool = context.curr_value;

                build_binary_expression(
                    &Number::IntConst(0),
                    rhs,
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                if let Some((tmp2, _)) = context.tmp_const {
                    context.tmp_const = Some((tmp1, tmp2));
                }
                let right_bool = context.curr_value;

                build_binary_expression_from_values(
                    program,
                    context,
                    BinaryOp::Or,
                    left_bool,
                    right_bool,
                )
            }
        }
    }
}

impl DumpIR for LAndExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            LAndExp::EqExp(e) => e.dump_ir(program, context),
            LAndExp::BinaryLAndExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::NotEq)?;
                let mut tmp1 = 0;
                if let Some((tmp, _)) = context.tmp_const {
                    tmp1 = tmp;
                }
                let left_bool = context.curr_value;

                build_binary_expression(
                    &Number::IntConst(0),
                    rhs,
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let right_bool = context.curr_value;
                if let Some((tmp2, _)) = context.tmp_const {
                    context.tmp_const = Some((tmp1, tmp2));
                }

                build_binary_expression_from_values(
                    program,
                    context,
                    BinaryOp::And,
                    left_bool,
                    right_bool,
                )
            }
        }
    }
}

impl DumpIR for EqExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            EqExp::RelExp(e) => e.dump_ir(program, context),
            EqExp::BinaryEqExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Eq)
            }
            EqExp::BinaryNeExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::NotEq)
            }
        }
    }
}

impl DumpIR for RelExp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            RelExp::AddExp(exp) => exp.dump_ir(program, context),
            RelExp::BinaryLtExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Lt)
            }
            RelExp::BinaryGtExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Gt)
            }
            RelExp::BinaryLeExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Le)
            }
            RelExp::BinaryGeExp(lhs, rhs) => {
                build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Ge)
            }
        }
    }
}

impl DumpIR for Number {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            Number::IntConst(i) => {
                if let Some(_) = context.tmp_const {
                    context.tmp_const = Some((*i, 0));
                    return Ok(());
                }

                let curr_func_data = program.func_mut(context.curr_func.unwrap());
                let new_value = curr_func_data.dfg_mut().new_value().integer(*i);
                context.curr_value = Some(new_value);
                Ok(())
            }
        }
    }
}
