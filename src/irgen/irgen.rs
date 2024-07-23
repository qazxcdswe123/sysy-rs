use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BasicBlock, BinaryOp, FunctionData, Program, Type, Value,
};

use crate::ast::{self, *};

use super::symtable::{SymTable, Symbol};

// use super::{new_value, IRContext};

// fn build_binary_expression(
//     first_exp: &dyn DumpIR,
//     second_exp: &dyn DumpIR,
//     program: &mut Program,
//     context: &mut IRContext,
//     op: BinaryOp,
// ) -> Result<(), String> {
//     first_exp.dump_ir(program, context)?;
//     let lhs = context.curr_value.unwrap();
//     second_exp.dump_ir(program, context)?;
//     let rhs = context.curr_value.unwrap();

//     build_binary_expression_from_value(program, context, op, lhs, rhs)?;

//     Ok(())
// }

// fn build_binary_expression_from_value(
//     program: &mut Program,
//     context: &mut IRContext,
//     op: BinaryOp,
//     lhs: koopa::ir::Value,
//     rhs: koopa::ir::Value,
// ) -> Result<(), String> {
//     let curr_func_data = program.func_mut(context.curr_func.unwrap());
//     let new_value = curr_func_data.dfg_mut().new_value().binary(op, lhs, rhs);
//     curr_func_data
//         .layout_mut()
//         .bb_mut(context.curr_block.unwrap())
//         .insts_mut()
//         .extend([new_value]);
//     context.curr_value = Some(new_value);
//     Ok(())
// }

// pub trait DumpIR {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String>;
// }

// impl DumpIR for CompUnit {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         self.func_def.dump_ir(program, context)
//     }
// }

// impl DumpIR for FuncDef {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         let ret_ty = match self.func_type {
//             FuncType::Int => Type::get_i32(),
//         };
//         let func = program.new_func(FunctionData::with_param_names(
//             "@".to_string() + self.ident.id.as_str(),
//             vec![],
//             ret_ty,
//         ));
//         let func_data = program.func_mut(func);
//         let new_block = func_data
//             .dfg_mut()
//             .new_bb()
//             .basic_block(Some("%entry".to_string()));
//         func_data.layout_mut().bbs_mut().extend([new_block]);
//         context.curr_block = Some(new_block);
//         context.curr_func = Some(func);
//         self.block.dump_ir(program, context)?;
//         Ok(())
//     }
// }

// impl DumpIR for Block {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         // self.stmt.dump_ir(program, context)?;
//         for ele in &self.items {
//             ele.dump_ir(program, context)?;
//         }
//         Ok(())
//     }
// }

// impl DumpIR for BlockItem {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             BlockItem::Decl(decl) => decl.dump_ir(program, context),
//             BlockItem::Stmt(stmt) => stmt.dump_ir(program, context),
//         }
//     }
// }

// impl DumpIR for Decl {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             Decl::ConstDecl(const_decl) => const_decl.dump_ir(program, context),
//             Decl::VarDecl(var_decl) => var_decl.dump_ir(program, context),
//         }
//     }
// }

// impl DumpIR for VarDecl {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         let btype = &self.btype;
//         for var_def in &self.var_defs {
//             match var_def {
//                 VarDef::WithoutInit(ident) => {
//                     let value = new_value(program, context).alloc(Type::get(btype.ty.clone()));
//                     program
//                         .func_mut(context.curr_func.unwrap())
//                         .dfg_mut()
//                         .set_value_name(value, Some(format!("@{}", ident.id)));
//                 }
//                 VarDef::WithInitVal(ident, rhs) => {
//                     let right_value = rhs.dump_ir(program, context)?;
//                 }
//             }
//         }
//         todo!()
//     }
// }

// impl DumpIR for InitVal {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         let InitVal::Exp(exp) = self;
//         exp.dump_ir(program, context)
//     }
// }

// impl DumpIR for ConstDecl {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         let ty = &self.btype;
//         for d in &self.const_defs {
//             let result = d.const_init_val.dump_ir(program, context)?;
//         }
//         todo!()
//     }
// }

// impl DumpIR for ConstInitVal {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         let ConstInitVal::ConstExp(exp) = self;
//         exp.dump_ir(program, context)
//     }
// }

// impl DumpIR for ConstExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         self.exp.dump_ir(program, context)
//     }
// }

// impl DumpIR for Stmt {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             Stmt::ReturnStmt(ret_exp) => {
//                 ret_exp.dump_ir(program, context)?;
//                 let curr_func_data = program.func_mut(context.curr_func.unwrap());
//                 let return_stmt = curr_func_data.dfg_mut().new_value().ret(context.curr_value);

//                 curr_func_data
//                     .layout_mut()
//                     .bb_mut(context.curr_block.unwrap())
//                     .insts_mut()
//                     .extend([return_stmt]);

//                 Ok(())
//             }
//             Stmt::AssignStmt(_, _) => todo!(),
//         }
//     }
// }

// impl DumpIR for Exp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             Exp::LOrExp(exp) => exp.dump_ir(program, context),
//         }
//     }
// }

// impl DumpIR for UnaryExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             UnaryExp::PrimaryExp(exp) => exp.dump_ir(program, context),
//             UnaryExp::PlusUnaryExp(exp) => exp.dump_ir(program, context),
//             UnaryExp::MinusUnaryExp(exp) => build_binary_expression(
//                 &Number::IntConst(0),
//                 &**exp,
//                 program,
//                 context,
//                 BinaryOp::Sub,
//             ),
//             UnaryExp::NotUnaryExp(exp) => build_binary_expression(
//                 &Number::IntConst(0),
//                 &**exp,
//                 program,
//                 context,
//                 BinaryOp::Eq,
//             ),
//         }
//     }
// }

// impl DumpIR for PrimaryExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             PrimaryExp::ParenExp(exp) => exp.dump_ir(program, context),
//             PrimaryExp::Number(n) => n.dump_ir(program, context),
//             PrimaryExp::LVal(lval) => lval.dump_ir(program, context),
//         }
//     }
// }

// impl DumpIR for LVal {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         todo!()
//     }
// }

// impl DumpIR for AddExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             AddExp::MulExp(m) => m.dump_ir(program, context),
//             AddExp::BinaryAddExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Add)
//             }
//             AddExp::BinarySubExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Sub)
//             }
//         }
//     }
// }

// impl DumpIR for MulExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             MulExp::UnaryExp(u) => u.dump_ir(program, context),
//             MulExp::BinaryMulExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Mul)
//             }
//             MulExp::BinaryDivExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Div)
//             }
//             MulExp::BinaryModExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Mod)
//             }
//         }
//     }
// }

// impl DumpIR for LOrExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             LOrExp::LAndExp(l) => l.dump_ir(program, context),
//             LOrExp::BinaryLOrExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::NotEq)?;
//                 let left_bool = context.curr_value.unwrap();
//                 build_binary_expression(
//                     &Number::IntConst(0),
//                     rhs,
//                     program,
//                     context,
//                     BinaryOp::NotEq,
//                 )?;
//                 let right_bool = context.curr_value.unwrap();
//                 build_binary_expression_from_value(
//                     program,
//                     context,
//                     BinaryOp::Or,
//                     left_bool,
//                     right_bool,
//                 )
//             }
//         }
//     }
// }

// impl DumpIR for LAndExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             LAndExp::EqExp(e) => e.dump_ir(program, context),
//             LAndExp::BinaryLAndExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::NotEq)?;
//                 let left_bool = context.curr_value.unwrap();
//                 build_binary_expression(
//                     &Number::IntConst(0),
//                     rhs,
//                     program,
//                     context,
//                     BinaryOp::NotEq,
//                 )?;
//                 let right_bool = context.curr_value.unwrap();
//                 build_binary_expression_from_value(
//                     program,
//                     context,
//                     BinaryOp::And,
//                     left_bool,
//                     right_bool,
//                 )
//             }
//         }
//     }
// }

// impl DumpIR for EqExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             EqExp::RelExp(e) => e.dump_ir(program, context),
//             EqExp::BinaryEqExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Eq)
//             }
//             EqExp::BinaryNeExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::NotEq)
//             }
//         }
//     }
// }

// impl DumpIR for RelExp {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             RelExp::AddExp(exp) => exp.dump_ir(program, context),
//             RelExp::BinaryLtExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Lt)
//             }
//             RelExp::BinaryGtExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Gt)
//             }
//             RelExp::BinaryLeExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Le)
//             }
//             RelExp::BinaryGeExp(lhs, rhs) => {
//                 build_binary_expression(&**lhs, rhs, program, context, BinaryOp::Ge)
//             }
//         }
//     }
// }

// impl DumpIR for Number {
//     fn dump_ir(&self, program: &mut Program, context: &mut IRContext) -> Result<(), String> {
//         match self {
//             Number::IntConst(i) => {
//                 let curr_func_data = program.func_mut(context.curr_func.unwrap());
//                 let new_value = curr_func_data.dfg_mut().new_value().integer(*i);
//                 context.curr_value = Some(new_value);
//                 Ok(())
//             }
//         }
//     }
// }

impl CompUnit {
    pub fn build_ir(self) -> Program {
        let mut symtable = SymTable::new();
        let mut program = Program::new();
        self.func_def.generate_program(&mut program, &mut symtable);
        program
    }
}

impl FuncDef {
    pub fn generate_program(self, program: &mut Program, symtable: &mut SymTable) {
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident.node),
            vec![],
            self.func_type.node.generate_program(),
        ));
        let func_data = program.func_mut(func);
        self.block.node.generate_program(func_data, symtable)
    }
}

impl Block {
    pub fn generate_program(self, func_data: &mut FunctionData, symtable: &mut SymTable) {
        let entry = func_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        for item in self.items {
            item.generate_program(func_data, symtable, entry);
        }
    }
}

impl BlockItem {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        match self {
            BlockItem::Decl { decl } => decl.generate_program(func_data, symtable, block),
            BlockItem::Stmt { stmt } => stmt.node.generate_program(func_data, symtable, block),
        }
    }
}

impl Stmt {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        match self {
            Stmt::AssignStmt { lhs, rhs } => {
                let var = symtable.get(&lhs.node);
                let var = match var {
                    Some(Symbol::Var(var)) => *var,
                    _ => unreachable!(),
                };
                let value = rhs.generate_program(func_data, symtable, block);
                let store = func_data.dfg_mut().new_value().store(value, var);

                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .push_key_back(store)
                    .unwrap();
            }
            Stmt::ReturnStmt(exp) => {
                let value = exp.unwrap().generate_program(func_data, symtable, block);
                let ret = func_data.dfg_mut().new_value().ret(Some(value));
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .push_key_back(ret)
                    .unwrap();
            }
        }
    }
}

impl Decl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        match self {
            Decl::Const(const_decl) => const_decl.node.generate_program(func_data, symtable, block),
            Decl::Var(var_decl) => var_decl.node.generate_program(func_data, symtable, block),
        }
    }
}

impl VarDecl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        for var_def in self.defs {
            var_def.generate_program(func_data, symtable, block, &self.ty.node);
        }
    }
}

impl VarDef {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
        ty: &BType,
    ) {
        let var = func_data.dfg_mut().new_value().alloc(ty.generate_program());
        func_data
            .layout_mut()
            .bb_mut(block)
            .insts_mut()
            .push_key_back(var)
            .unwrap();

        if let Some(init) = self.init {
            let init_val = init.node.generate_program(func_data, symtable, block);
            let store = func_data.dfg_mut().new_value().store(init_val, var);
            func_data
                .layout_mut()
                .bb_mut(block)
                .insts_mut()
                .push_key_back(store)
                .unwrap();
        }

        symtable.insert(self.ident.node, Symbol::Var(var))
    }
}

impl ConstDecl {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        for const_def in self.const_defs {
            const_def.generate_program(func_data, symtable, block);
        }
    }
}

impl ConstDef {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) {
        let value = self.const_init_val.const_propagation(symtable).unwrap();
        symtable.insert(self.ident.node, Symbol::Const(value))
    }
}

impl InitVal {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) -> Value {
        match self {
            InitVal::Exp(exp) => exp.generate_program(func_data, symtable, block),
        }
    }
}

impl Span<InitVal> {
    pub fn const_propagation(self, symtable: &SymTable) -> Result<i32, String> {
        match self.node {
            InitVal::Exp(exp) => exp.const_propagation(symtable),
        }
    }
}

impl ast::BinaryOp {
    pub fn generate_program(&self) -> BinaryOp {
        match self {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
            ast::BinaryOp::Mod => BinaryOp::Mod,
            ast::BinaryOp::Eq => BinaryOp::Eq,
            ast::BinaryOp::Ne => BinaryOp::NotEq,
            ast::BinaryOp::Lt => BinaryOp::Lt,
            ast::BinaryOp::Gt => BinaryOp::Gt,
            ast::BinaryOp::Le => BinaryOp::Le,
            ast::BinaryOp::Ge => BinaryOp::Ge,
            ast::BinaryOp::LAnd => BinaryOp::And,
            ast::BinaryOp::LOr => BinaryOp::Or,
        }
    }
}

impl Exp {
    pub fn generate_program(
        self,
        func_data: &mut FunctionData,
        symtable: &mut SymTable,
        block: BasicBlock,
    ) -> Value {
        match self {
            Exp::UnaryExp { op, exp } => {
                let value = exp.generate_program(func_data, symtable, block);
                match op.node {
                    UnaryOp::Minus => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let new_value =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Sub, zero, value);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([new_value]);
                        new_value
                    }
                    UnaryOp::Plus => value,
                    UnaryOp::Not => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let new_value =
                            func_data
                                .dfg_mut()
                                .new_value()
                                .binary(BinaryOp::Eq, zero, value);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([new_value]);
                        new_value
                    }
                }
            }
            Exp::BinaryExp { lhs, op, rhs } => {
                let lhs_value = lhs.generate_program(func_data, symtable, block);
                let rhs_value = rhs.generate_program(func_data, symtable, block);
                match op.node {
                    crate::ast::BinaryOp::LAnd => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let lhs_bool = func_data.dfg_mut().new_value().binary(
                            BinaryOp::NotEq,
                            zero,
                            lhs_value,
                        );
                        let rhs_bool = func_data.dfg_mut().new_value().binary(
                            BinaryOp::NotEq,
                            zero,
                            rhs_value,
                        );
                        let new_value = func_data.dfg_mut().new_value().binary(
                            BinaryOp::And,
                            lhs_bool,
                            rhs_bool,
                        );
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([lhs_bool, rhs_bool, new_value]);
                        new_value
                    }
                    crate::ast::BinaryOp::LOr => {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let lhs_bool = func_data.dfg_mut().new_value().binary(
                            BinaryOp::NotEq,
                            zero,
                            lhs_value,
                        );
                        let rhs_bool = func_data.dfg_mut().new_value().binary(
                            BinaryOp::NotEq,
                            zero,
                            rhs_value,
                        );
                        let new_value = func_data.dfg_mut().new_value().binary(
                            BinaryOp::Or,
                            lhs_bool,
                            rhs_bool,
                        );
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([lhs_bool, rhs_bool, new_value]);
                        new_value
                    }
                    _ => {
                        let new_value = func_data.dfg_mut().new_value().binary(
                            op.node.generate_program(),
                            lhs_value,
                            rhs_value,
                        );
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([new_value]);
                        new_value
                    }
                }
            }
            Exp::Number(n) => {
                let new_value = func_data.dfg_mut().new_value().integer(n.node);
                func_data
                    .layout_mut()
                    .bb_mut(block)
                    .insts_mut()
                    .extend([new_value]);
                new_value
            }
            Exp::LVal(lval) => {
                let var = symtable.get(&lval.node.ident.node).unwrap();
                match var {
                    Symbol::Var(value) => {
                        let load = func_data.dfg_mut().new_value().load(*value);
                        func_data
                            .layout_mut()
                            .bb_mut(block)
                            .insts_mut()
                            .extend([load]);
                        load
                    }
                    Symbol::Const(n) => func_data.dfg_mut().new_value().integer(*n),
                }
            }
        }
    }

    pub fn const_propagation(self, symtable: &SymTable) -> Result<i32, String> {
        Ok(match self {
            Exp::UnaryExp { op, exp } => {
                let exp = exp.const_propagation(symtable)?;
                match op.node {
                    UnaryOp::Plus => exp,
                    UnaryOp::Minus => -exp,
                    UnaryOp::Not => {
                        if exp == 0 {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
            Exp::BinaryExp { lhs, op, rhs } => {
                let lhs = lhs.const_propagation(symtable)?;
                let rhs = rhs.const_propagation(symtable)?;
                match op.node {
                    crate::ast::BinaryOp::Add => lhs + rhs,
                    crate::ast::BinaryOp::Sub => lhs - rhs,
                    crate::ast::BinaryOp::Mul => lhs * rhs,
                    crate::ast::BinaryOp::Div => lhs / rhs,
                    crate::ast::BinaryOp::Mod => lhs % rhs,
                    crate::ast::BinaryOp::LAnd => {
                        if lhs != 0 && rhs != 0 {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::LOr => {
                        if lhs != 0 || rhs != 0 {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::Eq => {
                        if lhs == rhs {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::Ne => {
                        if lhs != rhs {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::Lt => {
                        if lhs < rhs {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::Gt => {
                        if lhs > rhs {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::Le => {
                        if lhs <= rhs {
                            1
                        } else {
                            0
                        }
                    }
                    crate::ast::BinaryOp::Ge => {
                        if lhs >= rhs {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
            Exp::Number(n) => n.node,
            Exp::LVal(name) => {
                let var = symtable.get(&name.node.ident.node).unwrap();
                match var {
                    Symbol::Const(value) => *value,
                    _ => unreachable!(),
                }
            }
        })
    }
}

impl BType {
    pub fn generate_program(&self) -> Type {
        match self {
            BType::Int => Type::get_i32(),
        }
    }
}
