//! CompUnit      ::= [CompUnit] (Decl | FuncDef);
//!
//! Decl          ::= ConstDecl | VarDecl;
//! ConstDecl     ::= "const" BType ConstDef {"," ConstDef} ";";
//! BType         ::= "int";
//! ConstDef      ::= IDENT "=" ConstInitVal;
//! ConstInitVal  ::= ConstExp;
//! VarDecl       ::= BType VarDef {"," VarDef} ";";
//! VarDef        ::= IDENT | IDENT "=" InitVal;
//! InitVal       ::= Exp;
//!
//! FuncDef       ::= FuncType IDENT "(" [FuncFParams] ")" Block;
//! FuncType      ::= "void" | "int";
//! FuncFParams   ::= FuncFParam {"," FuncFParam};
//! FuncFParam    ::= BType IDENT;
//!
//! Block         ::= "{" {BlockItem} "}";
//! BlockItem     ::= Decl | Stmt;
//! Stmt          ::= LVal "=" Exp ";"
//!                 | [Exp] ";"
//!                 | Block
//!                 | "if" "(" Exp ")" Stmt ["else" Stmt]
//!                 | "while" "(" Exp ")" Stmt
//!                 | "break" ";"
//!                 | "continue" ";"
//!                 | "return" [Exp] ";";
//!
//! Exp           ::= LOrExp;
//! LVal          ::= IDENT;
//! PrimaryExp    ::= "(" Exp ")" | LVal | Number;
//! Number        ::= INT_CONST;
//! UnaryExp      ::= PrimaryExp | IDENT "(" [FuncRParams] ")" | UnaryOp UnaryExp;
//! UnaryOp       ::= "+" | "-" | "!";
//! FuncRParams   ::= Exp {"," Exp};
//! MulExp        ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
//! AddExp        ::= MulExp | AddExp ("+" | "-") MulExp;
//! RelExp        ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
//! EqExp         ::= RelExp | EqExp ("==" | "!=") RelExp;
//! LAndExp       ::= EqExp | LAndExp "&&" EqExp;
//! LOrExp        ::= LAndExp | LOrExp "||" LAndExp;
//! ConstExp      ::= Exp;

use koopa::ir::TypeKind;

pub struct CompUnit {
    pub func_def: FuncDef,
}

pub struct IDENT {
    pub id: String,
}

pub struct FuncDef {
    pub func_type: BType,
    pub ident: IDENT,
    pub block: Block,
    pub params: Vec<FuncFParam>,
}

pub struct FuncFParam {
    pub btype: BType,
    pub ident: IDENT,
}

pub struct BType {
    pub ty: TypeKind,
}

pub struct Block {
    pub items: Vec<BlockItem>,
}

pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

pub struct VarDecl {
    pub btype: BType,
    pub var_defs: Vec<VarDef>,
}

pub struct VarDef {
    pub ident: IDENT,
    pub init: Option<InitVal>,
}

pub enum InitVal {
    Exp(Exp),
}

pub struct ConstDecl {
    pub btype: BType,
    pub const_defs: Vec<ConstDef>,
}

pub struct ConstDef {
    pub ident: IDENT,
    pub const_init_val: ConstInitVal,
}

pub enum ConstInitVal {
    ConstExp(ConstExp),
}

pub struct ConstExp {
    pub exp: Exp,
}

pub enum BasicStmt {
    AssignStmt(LVal, Exp),
    ReturnStmt(Option<Exp>),
    ExpStmt(Option<Exp>),
    BlockStmt(Block),
    IfElseStmt(Exp, Box<BasicStmt>, Box<Option<BasicStmt>>),
    WhileStmt(Exp, Box<BasicStmt>),
    BreakStmt,
    ContinueStmt,
}

pub struct MatchedStmt {
    pub stmt: BasicStmt,
}

pub struct UnmatchedStmt {
    pub stmt: BasicStmt,
}

pub enum Stmt {
    MatchedStmt(MatchedStmt),
    UnmatchedStmt(UnmatchedStmt),
}

pub enum Exp {
    LOrExp(LOrExp),
}

pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    PlusUnaryExp(Box<UnaryExp>),
    MinusUnaryExp(Box<UnaryExp>),
    NotUnaryExp(Box<UnaryExp>),
    FuncCall(IDENT, Vec<Exp>),
}

pub enum PrimaryExp {
    ParenExp(Box<Exp>),
    Number(Number),
    LVal(LVal),
}

pub struct LVal {
    pub ident: IDENT,
}

pub enum Number {
    IntConst(i32),
}

pub enum AddExp {
    MulExp(MulExp),
    BinaryAddExp(Box<AddExp>, MulExp),
    BinarySubExp(Box<AddExp>, MulExp),
}

pub enum MulExp {
    UnaryExp(UnaryExp),
    BinaryMulExp(Box<MulExp>, UnaryExp),
    BinaryDivExp(Box<MulExp>, UnaryExp),
    BinaryModExp(Box<MulExp>, UnaryExp),
}

pub enum LOrExp {
    LAndExp(LAndExp),
    BinaryLOrExp(Box<LOrExp>, LAndExp),
}

pub enum LAndExp {
    EqExp(EqExp),
    BinaryLAndExp(Box<LAndExp>, EqExp),
}

pub enum EqExp {
    RelExp(RelExp),
    BinaryEqExp(Box<EqExp>, RelExp),
    BinaryNeExp(Box<EqExp>, RelExp),
}

pub enum RelExp {
    AddExp(AddExp),
    BinaryLtExp(Box<RelExp>, AddExp),
    BinaryGtExp(Box<RelExp>, AddExp),
    BinaryLeExp(Box<RelExp>, AddExp),
    BinaryGeExp(Box<RelExp>, AddExp),
}
