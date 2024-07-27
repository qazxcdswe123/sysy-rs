use koopa::ir::{
    builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder},
    BinaryOp, FunctionData, Program, Type,
};

use crate::{ast::*, irgen::insert_basic_blocks};

use super::{
    insert_instructions, new_block, new_value, DumpIR, DumpResult, ExpDumpIR, ExpDumpResult,
    IRContext, SymbolTableEntry,
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
        /*
           decl @getint(): i32
           decl @getch(): i32
           decl @getarray(*i32): i32
           decl @putint(i32)
           decl @putch(i32)
           decl @putarray(i32, *i32)
           decl @starttime()
           decl @stoptime()
        */
        context.symbol_tables.new_table();
        let stdlib = vec![
            ("getint", vec![], Type::get_i32()),
            ("getch", vec![], Type::get_i32()),
            (
                "getarray",
                vec![Type::get_pointer(Type::get_i32())],
                Type::get_i32(),
            ),
            ("putint", vec![Type::get_i32()], Type::get_unit()),
            ("putch", vec![Type::get_i32()], Type::get_unit()),
            (
                "putarray",
                vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
                Type::get_unit(),
            ),
            ("starttime", vec![], Type::get_unit()),
            ("stoptime", vec![], Type::get_unit()),
        ];
        for (name, params_ty, ret_ty) in stdlib {
            let func_data = FunctionData::new_decl(format!("@{}", name), params_ty, ret_ty);
            let func = program.new_func(func_data);
            context.function_table.insert(name.to_string(), func);
        }

        for unit in &self.units {
            unit.dump_ir(program, context)?;
        }
        Ok(DumpResult::Ok)
    }
}

impl DumpIR for Unit {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        match self {
            Unit::Decl(d) => d.dump_ir(program, context),
            Unit::FuncDef(f) => f.dump_ir(program, context),
        }
    }
}

impl DumpIR for FuncDef {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        let mut ir_params = Vec::<(Option<String>, Type)>::new();
        for param in &self.params {
            ir_params.push((
                Some(format!("%{}_param", param.ident.id)),
                Type::get(param.btype.ty.clone()),
            ))
        }
        let func_ret_ty = self.func_type.ty.clone();
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident.id),
            ir_params,
            Type::get(func_ret_ty),
        ));

        context.function_table.insert(self.ident.id.clone(), func);

        let func_data = program.func_mut(func);
        let new_block = func_data.dfg_mut().new_bb().basic_block(None);
        func_data.layout_mut().bbs_mut().extend([new_block]);
        context.curr_block = Some(new_block);
        context.curr_func = Some(func);

        context.symbol_tables.new_table();
        let parameter_count = program.func(context.curr_func.unwrap()).params().len();
        for idx in 0..parameter_count {
            let param = &self.params[idx];
            // curr_param
            let arguments = program.func(context.curr_func.unwrap()).params()[idx];
            // param_value
            let parameters = new_value(program, context).alloc(Type::get(param.btype.ty.clone()));
            program
                .func_mut(context.curr_func.unwrap())
                .dfg_mut()
                .set_value_name(
                    parameters,
                    Some(format!(
                        "%{}_{}param",
                        param.ident.id,
                        context.symbol_tables.depth()
                    )),
                );

            context.symbol_tables.insert(
                param.ident.id.clone(),
                SymbolTableEntry::Variable(param.btype.ty.clone(), parameters),
            );
            let assign_inst = new_value(program, context).store(arguments, parameters);
            insert_instructions(program, context, [parameters, assign_inst]);
        }
        match self.block.dump_ir(program, context)? {
            DumpResult::Ok => {
                let ret_inst = new_value(program, context).ret(None);
                insert_instructions(program, context, [ret_inst]);
            }
            DumpResult::Abort => {}
        }
        context.curr_func = None;
        context.curr_block = None;
        Ok(DumpResult::Ok)
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
            // early return in block
            if let DumpResult::Abort = res {
                block_res = DumpResult::Abort;
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
            let final_var_ptr = if let Some(func) = context.curr_func {
                // local function
                let var_ptr = new_value(program, context).alloc(Type::get(btype.ty.clone()));
                program.func_mut(func).dfg_mut().set_value_name(
                    var_ptr,
                    Some(format!(
                        "@{}_{}",
                        var_def.ident.id,
                        context.symbol_tables.depth()
                    )),
                );
                insert_instructions(program, context, [var_ptr]);

                if let Some(rhs) = &var_def.init {
                    let res = rhs.dump_ir(program, context)?;
                    let rhs_value = match res {
                        ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                        ExpDumpResult::Value(v) => v,
                    };

                    let store_inst = new_value(program, context).store(rhs_value, var_ptr);
                    insert_instructions(program, context, [store_inst]);
                }
                var_ptr
            } else {
                // global
                let var_ptr = match &var_def.init {
                    Some(rhs) => match rhs.dump_ir(program, context)? {
                        ExpDumpResult::Const(c) => {
                            let init = program.new_value().integer(c);
                            program.new_value().global_alloc(init)
                        }
                        ExpDumpResult::Value(v) => program.new_value().global_alloc(v),
                    },
                    None => {
                        let zero_init = program.new_value().zero_init(Type::get(btype.ty.clone()));
                        program.new_value().global_alloc(zero_init)
                    }
                };
                program.set_value_name(
                    var_ptr,
                    Some(format!(
                        "@{}_{}",
                        var_def.ident.id,
                        context.symbol_tables.depth()
                    )),
                );
                var_ptr
            };

            context.symbol_tables.insert(
                var_def.ident.id.clone(),
                SymbolTableEntry::Variable(btype.ty.clone(), final_var_ptr),
            );
        }
        Ok(DumpResult::Ok)
    }
}

// fn vardecl_common(
//     program: &mut Program,
//     context: &mut IRContext,
//     btype: &BType,
//     var_def: &VarDef,
// ) -> koopa::ir::Value {
//     let dest = new_value(program, context).alloc(Type::get(btype.ty.clone()));
//     program
//         .func_mut(context.curr_func.unwrap())
//         .dfg_mut()
//         .set_value_name(
//             dest,
//             Some(format!(
//                 "@{}_{}",
//                 var_def.ident.id,
//                 context.symbol_tables.depth()
//             )),
//         );
//     insert_instructions(program, context, [dest]);
//     dest
// }

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
            Stmt::MatchedStmt(stmt) => stmt.dump_ir(program, context),
            Stmt::UnmatchedStmt(stmt) => stmt.dump_ir(program, context),
        }
    }
}

impl DumpIR for UnmatchedStmt {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        self.stmt.dump_ir(program, context)
    }
}

impl DumpIR for MatchedStmt {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        self.stmt.dump_ir(program, context)
    }
}

impl DumpIR for BasicStmt {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<DumpResult, String> {
        match &self {
            BasicStmt::ReturnStmt(ret_exp) => {
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
            BasicStmt::AssignStmt(lval, rhs_exp) => {
                let res1 = lval.dump_ir(program, context)?;
                let lval_dest = match res1 {
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

                let store_inst = new_value(program, context).store(rhs_value, lval_dest);
                insert_instructions(program, context, [store_inst]);
                Ok(DumpResult::Ok)
            }
            BasicStmt::ExpStmt(e) => {
                if let Some(exp) = e {
                    exp.dump_ir(program, context)?;
                    Ok(DumpResult::Ok)
                } else {
                    Ok(DumpResult::Ok)
                }
            }
            BasicStmt::BlockStmt(b) => b.dump_ir(program, context),
            BasicStmt::IfElseStmt(cond, s1, option_s2) => {
                let cond_val = match cond.dump_ir(program, context)? {
                    ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                    ExpDumpResult::Value(v) => v,
                };
                let if_block_end = program
                    .func_mut(context.curr_func.unwrap())
                    .dfg_mut()
                    .new_bb()
                    .basic_block(Some(format!("%if_block_end_bb{}", context.bb)));
                context.bb += 1;

                let if_block_start = context.curr_block.unwrap();

                let then_block = program
                    .func_mut(context.curr_func.unwrap())
                    .dfg_mut()
                    .new_bb()
                    .basic_block(Some(format!("%then_bb{}", context.bb)));
                context.bb += 1;
                insert_basic_blocks(program, context, [then_block]);
                context.curr_block = Some(then_block);

                match s1.dump_ir(program, context)? {
                    DumpResult::Ok => {
                        let jmp_inst = new_value(program, context).jump(if_block_end);
                        insert_instructions(program, context, [jmp_inst]);
                    }
                    DumpResult::Abort => {}
                }
                let else_block = match &**option_s2 {
                    Some(stmt2) => {
                        let else_block = program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .new_bb()
                            .basic_block(Some(format!("%else_bb{}", context.bb)));
                        context.bb += 1;
                        insert_basic_blocks(program, context, [else_block]);
                        context.curr_block = Some(else_block);
                        match stmt2.dump_ir(program, context)? {
                            DumpResult::Ok => {
                                let jmp_inst = new_value(program, context).jump(if_block_end);
                                insert_instructions(program, context, [jmp_inst]);
                            }
                            DumpResult::Abort => {}
                        }
                        else_block
                    }
                    None => if_block_end,
                };
                let if_stmt = new_value(program, context).branch(cond_val, then_block, else_block);
                context.curr_block = Some(if_block_start);
                insert_instructions(program, context, [if_stmt]);

                context.curr_block = Some(if_block_end);
                insert_basic_blocks(program, context, [if_block_end]);

                Ok(DumpResult::Ok)
            }
            BasicStmt::WhileStmt(cond, stmt) => {
                let while_block_start = new_block(program, context, "while_start");
                let while_block_body = new_block(program, context, "while_body");
                let while_block_end = new_block(program, context, "while_end");
                insert_basic_blocks(
                    program,
                    context,
                    [while_block_start, while_block_body, while_block_end],
                );

                let jmp_while_start_inst = new_value(program, context).jump(while_block_start);
                insert_instructions(program, context, [jmp_while_start_inst]);

                context.curr_block = Some(while_block_start);
                let cond_val = match cond.dump_ir(program, context)? {
                    ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                    ExpDumpResult::Value(v) => v,
                };
                let while_stmt =
                    new_value(program, context).branch(cond_val, while_block_body, while_block_end);
                insert_instructions(program, context, [while_stmt]);

                context.curr_block = Some(while_block_body);
                context.break_blocks.push(while_block_end);
                context.continue_blocks.push(while_block_start);

                match stmt.dump_ir(program, context)? {
                    DumpResult::Ok => {
                        let jmp_while_start_inst =
                            new_value(program, context).jump(while_block_start);
                        insert_instructions(program, context, [jmp_while_start_inst]);
                    }
                    DumpResult::Abort => {}
                }

                context.curr_block = Some(while_block_end);
                context.break_blocks.pop();
                context.continue_blocks.pop();
                Ok(DumpResult::Ok)
            }
            BasicStmt::BreakStmt => {
                let break_target = match context.break_blocks.last() {
                    Some(b) => *b,
                    None => return Err("Break statement outside loop".to_string()),
                };
                let break_inst = new_value(program, context).jump(break_target);
                insert_instructions(program, context, [break_inst]);
                Ok(DumpResult::Abort)
            }
            BasicStmt::ContinueStmt => {
                let continue_target = match context.continue_blocks.last() {
                    Some(b) => *b,
                    None => return Err("Continue statement outside loop".to_string()),
                };
                let continue_inst = new_value(program, context).jump(continue_target);
                insert_instructions(program, context, [continue_inst]);
                Ok(DumpResult::Abort)
            }
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
            UnaryExp::FuncCall(func_id, params) => {
                let func = context
                    .function_table
                    .get(&func_id.id)
                    .cloned()
                    .ok_or_else(|| format!("Function {} not found", &func_id.id))?;

                let mut ir_params = Vec::<koopa::ir::Value>::new();
                for param in params {
                    ir_params.push(match param.dump_ir(program, context)? {
                        ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                        ExpDumpResult::Value(v) => v,
                    });
                }

                let call_inst = new_value(program, context).call(func, ir_params);
                insert_instructions(program, context, [call_inst]);

                Ok(ExpDumpResult::Value(call_inst))
            }
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

/*
   int result = 1;
   if (lhs == 0) {
       result = rhs!=0;
   }
*/
impl ExpDumpIR for LOrExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            LOrExp::LAndExp(l) => l.dump_ir(program, context),
            LOrExp::BinaryLOrExp(lhs, rhs) => {
                let res1 = lhs.dump_ir(program, context)?;
                match res1 {
                    ExpDumpResult::Const(c) => {
                        if c != 0 {
                            return Ok(ExpDumpResult::Const(1));
                        } else {
                            build_binary_expression_from_results(
                                ExpDumpResult::Const(0),
                                rhs.dump_ir(program, context)?,
                                program,
                                context,
                                BinaryOp::NotEq,
                            )
                        }
                    }
                    ExpDumpResult::Value(v) => {
                        let block1 = program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .new_bb()
                            .basic_block(Some(format!("%LOr_if_block_1_bb{}", context.bb)));
                        context.bb += 1;
                        let block_end = program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .new_bb()
                            .basic_block(Some(format!("%LOr_block_end_bb{}", context.bb)));
                        context.bb += 1;
                        insert_basic_blocks(program, context, [block1, block_end]);

                        let res_ptr = new_value(program, context).alloc(Type::get_i32());
                        program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .set_value_name(res_ptr, Some(format!("%LOr_result")));
                        let one = new_value(program, context).integer(1);
                        let zero = new_value(program, context).integer(0);
                        let store = new_value(program, context).store(one, res_ptr);

                        let cond = new_value(program, context).binary(BinaryOp::Eq, v, zero); // v == 0, implies false
                        let branch = new_value(program, context).branch(cond, block1, block_end);
                        insert_instructions(program, context, [res_ptr, store, cond, branch]);

                        context.curr_block = Some(block1);

                        let res2 = build_binary_expression_from_results(
                            ExpDumpResult::Const(0),
                            rhs.dump_ir(program, context)?,
                            program,
                            context,
                            BinaryOp::NotEq,
                        )?;
                        let value2 = match res2 {
                            ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                            ExpDumpResult::Value(v) => v,
                        };
                        let store2 = new_value(program, context).store(value2, res_ptr);
                        let jmp_inst = new_value(program, context).jump(block_end);
                        insert_instructions(program, context, [store2, jmp_inst]);

                        context.curr_block = Some(block_end);
                        let load = new_value(program, context).load(res_ptr);
                        insert_instructions(program, context, [load]);
                        Ok(ExpDumpResult::Value(load))
                    }
                }
            }
        }
    }
}

/*
   int result = 0;
   if (lhs != 0) {
       result = rhs!=0;
   }
*/
impl ExpDumpIR for LAndExp {
    fn dump_ir(
        &self,
        program: &mut Program,
        context: &mut IRContext,
    ) -> Result<ExpDumpResult, String> {
        match self {
            LAndExp::EqExp(e) => e.dump_ir(program, context),
            LAndExp::BinaryLAndExp(lhs, rhs) => {
                let res1 = lhs.dump_ir(program, context)?;
                match res1 {
                    ExpDumpResult::Const(c) => {
                        if c != 0 {
                            build_binary_expression_from_results(
                                ExpDumpResult::Const(0),
                                rhs.dump_ir(program, context)?,
                                program,
                                context,
                                BinaryOp::NotEq,
                            )
                        } else {
                            Ok(ExpDumpResult::Const(0))
                        }
                    }
                    ExpDumpResult::Value(v) => {
                        let then_block = program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .new_bb()
                            .basic_block(Some(format!("%LAnd_if_block_then_bb{}", context.bb)));
                        let block_end = program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .new_bb()
                            .basic_block(Some(format!("%LAnd_block_end_bb{}", context.bb)));
                        insert_basic_blocks(program, context, [then_block, block_end]);

                        let res_ptr = new_value(program, context).alloc(Type::get_i32());
                        program
                            .func_mut(context.curr_func.unwrap())
                            .dfg_mut()
                            .set_value_name(res_ptr, Some(format!("%LAnd_result")));
                        let zero = new_value(program, context).integer(0);
                        let store = new_value(program, context).store(zero, res_ptr);
                        let cond = new_value(program, context).binary(BinaryOp::Eq, v, zero);
                        let branch =
                            new_value(program, context).branch(cond, block_end, then_block);
                        insert_instructions(program, context, [res_ptr, store, cond, branch]);
                        context.curr_block = Some(then_block);

                        let res2 = build_binary_expression_from_results(
                            ExpDumpResult::Const(0),
                            rhs.dump_ir(program, context)?,
                            program,
                            context,
                            BinaryOp::NotEq,
                        )?;
                        let value2 = match res2 {
                            ExpDumpResult::Const(c) => new_value(program, context).integer(c),
                            ExpDumpResult::Value(v) => v,
                        };
                        let store2 = new_value(program, context).store(value2, res_ptr);
                        let jmp_inst = new_value(program, context).jump(block_end);
                        insert_instructions(program, context, [store2, jmp_inst]);

                        context.curr_block = Some(block_end);
                        let load = new_value(program, context).load(res_ptr);
                        insert_instructions(program, context, [load]);
                        Ok(ExpDumpResult::Value(load))
                    }
                }
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
