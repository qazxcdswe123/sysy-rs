use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type,
};

use crate::ast::*;

use super::IRContext;

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
        let ret_ty = match self.func_type {
            FuncType::Int => Type::get_i32(),
        };
        let func = program.new_func(FunctionData::with_param_names(
            "@".to_string() + self.ident.as_str(),
            vec![],
            ret_ty,
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
        self.stmt.dump_ir(program, context)?;
        Ok(())
    }
}

impl DumpIR for Stmt {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            Stmt::Return(ret_exp) => {
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
        }
    }
}

impl DumpIR for Exp {
    fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
        match self {
            Exp::UnaryExp(u) => u.dump_ir(program, context),
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
