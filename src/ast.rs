// use koopa::ir::TypeKind;

// pub struct CompUnit {
//     pub func_def: FuncDef,
// }

// pub struct IDENT {
//     pub id: String,
// }

// pub struct FuncDef {
//     pub func_type: BType,
//     pub ident: IDENT,
//     pub block: Block,
// }

// pub struct BType {
//     pub ty: TypeKind,
// }

// pub struct Block {
//     pub items: Vec<BlockItem>,
// }

// pub enum BlockItem {
//     Decl(Decl),
//     Stmt(Stmt),
// }

// pub enum Decl {
//     ConstDecl(ConstDecl),
//     VarDecl(VarDecl),
// }

// pub struct VarDecl {
//     pub btype: BType,
//     pub var_defs: Vec<VarDef>,
// }

// pub enum VarDef {
//     WithoutInit(IDENT),
//     WithInitVal(IDENT, InitVal),
// }

// pub enum InitVal {
//     Exp(Exp),
// }

// pub struct ConstDecl {
//     pub btype: BType,
//     pub const_defs: Vec<ConstDef>,
// }

// pub struct ConstDef {
//     pub ident: IDENT,
//     pub const_init_val: ConstInitVal,
// }

// pub enum ConstInitVal {
//     ConstExp(ConstExp),
// }

// pub struct ConstExp {
//     pub exp: Exp,
// }

// pub enum Stmt {
//     ReturnStmt(Exp),
//     AssignStmt(LVal, Exp),
// }

// pub enum Exp {
//     LOrExp(LOrExp),
// }

// pub enum UnaryExp {
//     PrimaryExp(PrimaryExp),
//     PlusUnaryExp(Box<UnaryExp>),
//     MinusUnaryExp(Box<UnaryExp>),
//     NotUnaryExp(Box<UnaryExp>),
// }

// pub enum PrimaryExp {
//     ParenExp(Box<Exp>),
//     Number(Number),
//     LVal(LVal),
// }

// pub struct LVal {
//     pub ident: IDENT,
// }

// pub enum Number {
//     IntConst(i32),
// }

// pub enum AddExp {
//     MulExp(MulExp),
//     BinaryAddExp(Box<AddExp>, MulExp),
//     BinarySubExp(Box<AddExp>, MulExp),
// }

// pub enum MulExp {
//     UnaryExp(UnaryExp),
//     BinaryMulExp(Box<MulExp>, UnaryExp),
//     BinaryDivExp(Box<MulExp>, UnaryExp),
//     BinaryModExp(Box<MulExp>, UnaryExp),
// }

// pub enum LOrExp {
//     LAndExp(LAndExp),
//     BinaryLOrExp(Box<LOrExp>, LAndExp),
// }

// pub enum LAndExp {
//     EqExp(EqExp),
//     BinaryLAndExp(Box<LAndExp>, EqExp),
// }

// pub enum EqExp {
//     RelExp(RelExp),
//     BinaryEqExp(Box<EqExp>, RelExp),
//     BinaryNeExp(Box<EqExp>, RelExp),
// }

// pub enum RelExp {
//     AddExp(AddExp),
//     BinaryLtExp(Box<RelExp>, AddExp),
//     BinaryGtExp(Box<RelExp>, AddExp),
//     BinaryLeExp(Box<RelExp>, AddExp),
//     BinaryGeExp(Box<RelExp>, AddExp),
// }

use std::ops::Range;

#[derive(Clone)]
pub struct Span<T> {
    pub node: T,
    pub start: usize,
    pub end: usize,
}

pub trait Spanned {
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn span(&self) -> Range<usize> {
        self.start()..self.end()
    }
}

pub trait IntoSpan {
    fn into_span(self, start: usize, end: usize) -> Span<Self>
    where
        Self: Sized,
    {
        Span {
            node: self,
            start,
            end,
        }
    }
}

impl<T> Spanned for Span<T> {
    fn start(&self) -> usize {
        self.start
    }
    fn end(&self) -> usize {
        self.end
    }
}

pub struct CompUnit {
    pub func_def: FuncDef,
}

impl Spanned for CompUnit {
    fn start(&self) -> usize {
        self.func_def.start()
    }

    fn end(&self) -> usize {
        self.func_def.end()
    }
}

pub struct FuncDef {
    pub func_type: Span<BType>,
    pub ident: Span<String>,
    pub block: Span<Block>,
}

impl Spanned for FuncDef {
    fn end(&self) -> usize {
        self.block.end()
    }

    fn start(&self) -> usize {
        self.func_type.start()
    }
}

#[derive(Clone)]
pub enum BType {
    Int,
}

impl IntoSpan for BType {}

pub struct Block {
    pub items: Vec<BlockItem>,
}

impl IntoSpan for Block {}

#[derive(Clone)]
pub enum BlockItem {
    Decl { decl: Decl },
    Stmt { stmt: Span<Stmt> },
}

impl Spanned for BlockItem {
    fn start(&self) -> usize {
        match self {
            BlockItem::Decl { decl } => decl.start(),
            BlockItem::Stmt { stmt } => stmt.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            BlockItem::Decl { decl } => decl.end(),
            BlockItem::Stmt { stmt } => stmt.end(),
        }
    }
}

#[derive(Clone)]
pub enum Decl {
    Const(Span<ConstDecl>),
    Var(Span<VarDecl>),
}

impl Spanned for Decl {
    fn start(&self) -> usize {
        match self {
            Decl::Const(decl) => decl.start(),
            Decl::Var(decl) => decl.start(),
        }
    }

    fn end(&self) -> usize {
        match self {
            Decl::Const(decl) => decl.end(),
            Decl::Var(decl) => decl.end(),
        }
    }
}

#[derive(Clone)]
pub struct ConstDecl {
    pub ty: Span<BType>,
    pub defs: Vec<ConstDef>,
}

impl IntoSpan for ConstDecl {}

#[derive(Clone)]
pub struct VarDecl {
    pub ty: Span<BType>,
    pub defs: Vec<VarDef>,
}

impl IntoSpan for VarDecl {}

#[derive(Clone)]
pub struct VarDef {
    pub ident: Span<String>,
    pub init: Option<Span<InitVal>>,
}

#[derive(Clone)]
pub struct ConstDef {
    pub ident: Span<String>,
    pub init: Span<InitVal>,
}

#[derive(Clone)]
pub enum InitVal {
    Exp(Exp),
}

impl IntoSpan for InitVal {}

#[derive(Clone)]
pub enum Exp {
    UnaryExp {
        op: Span<UnaryOp>,
        exp: Box<Exp>,
    },
    BinaryExp {
        lhs: Box<Exp>,
        op: Span<BinaryOp>,
        rhs: Box<Exp>,
    },
    Number(Span<i32>),
    LVal(Span<LVal>),
}

#[derive(Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

impl IntoSpan for UnaryOp {}

#[derive(Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LAnd,
    LOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl IntoSpan for BinaryOp {}

#[derive(Clone)]
pub struct LVal {
    pub ident: Span<String>,
}

impl IntoSpan for LVal {}

#[derive(Clone)]
pub enum Stmt {
    ReturnStmt(Option<Exp>),
    AssignStmt { lhs: Span<LVal>, rhs: Exp },
    // BlockStmt { block: Span<Block> },
    // ExpStmt { exp: Exp },
}

impl IntoSpan for Stmt {}

impl IntoSpan for String {}

impl IntoSpan for i32 {}
