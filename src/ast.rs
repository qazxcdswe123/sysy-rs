/// CompUnit  ::= FuncDef;
///
/// FuncDef   ::= FuncType IDENT "(" ")" Block;
/// FuncType  ::= "int";
///
/// Block     ::= "{" Stmt "}";
/// Stmt        ::= "return" Exp ";";
/// Exp         ::= AddExp;
/// PrimaryExp  ::= "(" Exp ")" | Number;
/// Number      ::= INT_CONST;
/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
/// UnaryOp     ::= "+" | "-" | "!";
/// MulExp      ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
/// AddExp      ::= MulExp | AddExp ("+" | "-") MulExp;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp {
    AddExp(AddExp)
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    PlusUnaryExp(Box<UnaryExp>),
    MinusUnaryExp(Box<UnaryExp>),
    NotUnaryExp(Box<UnaryExp>),
}

#[derive(Debug)]
pub enum PrimaryExp {
    ParenExp(Box<Exp>),
    Number(Number),
}

#[derive(Debug)]
pub enum Number {
    IntConst(i32),
}

#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    BinaryAddExp(Box<AddExp>, MulExp),
    BinarySubExp(Box<AddExp>, MulExp),
}

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    BinaryMulExp(Box<MulExp>, UnaryExp),
    BinaryDivExp(Box<MulExp>, UnaryExp),
    BinaryModExp(Box<MulExp>, UnaryExp),
}
