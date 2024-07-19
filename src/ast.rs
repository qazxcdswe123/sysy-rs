/// CompUnit  ::= FuncDef;
///
/// FuncDef   ::= FuncType IDENT "(" ")" Block;
/// FuncType  ::= "int";
///
/// Block         ::= "{" {BlockItem} "}";
/// BlockItem     ::= Decl | Stmt;
/// Stmt        ::= "return" Exp ";";
/// Exp         ::= LOrExp;
/// PrimaryExp    ::= "(" Exp ")" | LVal | Number;
/// ConstExp      ::= Exp;
/// Number      ::= INT_CONST;
/// LVal          ::= IDENT;
/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
/// UnaryOp     ::= "+" | "-" | "!";
/// MulExp      ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
/// AddExp      ::= MulExp | AddExp ("+" | "-") MulExp;
/// RelExp      ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
/// EqExp       ::= RelExp | EqExp ("==" | "!=") RelExp;
/// LAndExp     ::= EqExp | LAndExp "&&" EqExp;
/// LOrExp      ::= LAndExp | LOrExp "||" LAndExp;
///
/// Decl          ::= ConstDecl;
/// ConstDecl     ::= "const" FuncType ConstDef {"," ConstDef} ";";
/// ConstDef      ::= IDENT "=" ConstInitVal;
/// ConstInitVal  ::= ConstExp;

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
    pub items: Vec<BlockItem>,
}

pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

pub enum Decl {
    ConstDecl(ConstDecl),
}

pub struct ConstDecl {
    pub btype: FuncType,
    pub const_defs: Vec<ConstDef>,
}

pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

pub enum ConstInitVal {
    ConstExp(ConstExp),
}

pub struct ConstExp {
    pub exp: Exp,
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
    LVal(LVal),
}

pub struct LVal {
    pub ident: String,
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
