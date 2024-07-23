use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type,
};

use crate::ast::*;

use super::{new_value, IRContext};

fn build_binary_expression(
    first_exp: &dyn DumpIR,
    second_exp: &dyn DumpIR,
    program: &mut Program,
    context: &mut IRContext,
    op: BinaryOp,
) -> Result<(), String> {
    first_exp.dump_ir(program, context)?;
    let lhs = context.curr_value.unwrap();
    second_exp.dump_ir(program, context)?;
    let rhs = context.curr_value.unwrap();

    build_binary_expression_from_value(program, context, op, lhs, rhs)?;

    Ok(())
}

fn build_binary_expression_from_value(
    program: &mut Program,
    context: &mut IRContext,
    op: BinaryOp,
    lhs: koopa::ir::Value,
    rhs: koopa::ir::Value,
) -> Result<(), String> {
    let curr_func_data = program.func_mut(context.curr_func.unwrap());
    let new_value = curr_func_data.dfg_mut().new_value().binary(op, lhs, rhs);
    curr_func_data
        .layout_mut()
        .bb_mut(context.curr_block.unwrap())
        .insts_mut()
        .extend([new_value]);
    context.curr_value = Some(new_value);
    Ok(())
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
        // self.stmt.dump_ir(program, context)?;
        for ele in &self.items {
            ele.dump_ir(program, context)?;
        }
        Ok(())
    }
}

impl DumpIR for BlockItem {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            BlockItem::Decl(decl) => decl.dump_ir(program, context),
            BlockItem::Stmt(stmt) => stmt.dump_ir(program, context),
        }
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
                VarDef::WithoutInit(ident) => {
                    let value = new_value(program, context).alloc(Type::get(btype.ty.clone()));
                    program
                        .func_mut(context.curr_func.unwrap())
                        .dfg_mut()
                        .set_value_name(value, Some(format!("@{}", ident.id)));
                }
                VarDef::WithInitVal(ident, rhs) => {
                    let right_value = rhs.dump_ir(program, context)?;
                }
            }
        }
        todo!()
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
        let ty = &self.btype;
        for d in &self.const_defs {
            let result = d.const_init_val.dump_ir(program, context)?;
        }
        todo!()
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
            Stmt::AssignStmt(_, _) => todo!(),
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
            PrimaryExp::LVal(lval) => lval.dump_ir(program, context),
        }
    }
}

impl DumpIR for LVal {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        todo!()
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
                let left_bool = context.curr_value.unwrap();
                build_binary_expression(
                    &Number::IntConst(0),
                    rhs,
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let right_bool = context.curr_value.unwrap();
                build_binary_expression_from_value(
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
                let left_bool = context.curr_value.unwrap();
                build_binary_expression(
                    &Number::IntConst(0),
                    rhs,
                    program,
                    context,
                    BinaryOp::NotEq,
                )?;
                let right_bool = context.curr_value.unwrap();
                build_binary_expression_from_value(
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
                let curr_func_data = program.func_mut(context.curr_func.unwrap());
                let new_value = curr_func_data.dfg_mut().new_value().integer(*i);
                context.curr_value = Some(new_value);
                Ok(())
            }
        }
    }
}
