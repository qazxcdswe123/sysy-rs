/// CompUnit  ::= FuncDef;
///
/// FuncDef   ::= FuncType IDENT "(" ")" Block;
/// FuncType  ::= "int";
///
/// Block     ::= "{" Stmt "}";
/// Stmt        ::= "return" Exp ";";
/// Exp         ::= LOrExp;
/// PrimaryExp  ::= "(" Exp ")" | Number;
/// Number      ::= INT_CONST;
/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
/// UnaryOp     ::= "+" | "-" | "!";
/// MulExp      ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
/// AddExp      ::= MulExp | AddExp ("+" | "-") MulExp;
/// RelExp      ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
/// EqExp       ::= RelExp | EqExp ("==" | "!=") RelExp;
/// LAndExp     ::= EqExp | LAndExp "&&" EqExp;
/// LOrExp      ::= LAndExp | LOrExp "||" LAndExp;

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
    LOrExp(LOrExp),
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
