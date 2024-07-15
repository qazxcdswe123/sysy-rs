/// CompUnit  ::= FuncDef;
///
/// FuncDef   ::= FuncType IDENT "(" ")" Block;
/// FuncType  ::= "int";
///
/// Block     ::= "{" Stmt "}";
/// Stmt        ::= "return" Exp ";";
/// Exp         ::= UnaryExp;
/// PrimaryExp  ::= "(" Exp ")" | Number;
/// Number      ::= INT_CONST;
/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
/// UnaryOp     ::= "+" | "-" | "!";

pub struct CompUnit {
    pub func_def: FuncDef,
}

pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

pub enum FuncType {
    Int,
}

pub struct Block {
    pub stmt: Stmt,
}

pub enum Stmt {
    Return(Exp),
}

pub enum Exp {
    UnaryExp(UnaryExp),
}

pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    PlusUnaryExp(Box<UnaryExp>),
    MinusUnaryExp(Box<UnaryExp>),
    NotUnaryExp(Box<UnaryExp>),
}

pub enum PrimaryExp {
    ParenExp(Box<Exp>),
    Number(Number),
}

pub enum Number {
    IntConst(i32),
}
