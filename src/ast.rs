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

static mut CUR_VAR_ID: u32 = 0;

pub trait Dump {
    fn dump(&self) -> String;
}

pub struct CompUnit {
    pub func_def: FuncDef,
}

impl Dump for CompUnit {
    fn dump(&self) -> String {
        self.func_def.dump()
    }
}

pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl Dump for FuncDef {
    fn dump(&self) -> String {
        let func_string = format!("fun @{}(): ", self.ident);
        func_string + &self.func_type.dump() + &self.block.dump()
    }
}

pub enum FuncType {
    Int,
}

impl Dump for FuncType {
    fn dump(&self) -> String {
        match self {
            FuncType::Int => "i32 ".to_string(),
        }
    }
}

pub struct Block {
    pub stmt: Stmt,
}

impl Dump for Block {
    fn dump(&self) -> String {
        format!("{{\n%entry:\n{}\n}}\n", self.stmt.dump())
    }
}

pub enum Stmt {
    Return(ReturnStmt),
}

impl Dump for Stmt {
    fn dump(&self) -> String {
        match self {
            Self::Return(r) => r.dump(),
        }
    }
}

pub struct ReturnStmt {
    pub exp: Exp,
}

impl Dump for ReturnStmt {
    fn dump(&self) -> String {
        self.exp.dump() + "ret\n\n"
    }
}

pub enum Exp {
    UnaryExp(UnaryExp),
}

impl Dump for Exp {
    fn dump(&self) -> String {
        match self {
            Self::UnaryExp(u) => u.dump(),
        }
    }
}

pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    Unary(UnaryOp, Box<UnaryExp>),
}

impl Dump for UnaryExp {
    fn dump(&self) -> String {
        let mut res = String::new();
        match self {
            UnaryExp::PrimaryExp(p) => {
                return p.dump();
            }
            UnaryExp::Unary(op, exp) => match exp.as_ref() {
                UnaryExp::PrimaryExp(PrimaryExp::Number(num)) => {
                    res.push_str(&format!("%{}=", unsafe { CUR_VAR_ID }));
                    res.push_str(op.dump().as_str());
                    res.push_str(&format!("{}\n", num));
                    unsafe {
                        CUR_VAR_ID += 1;
                    }
                }
                UnaryExp::Unary(_, rec_exp) => {
                    res.push_str(rec_exp.dump().as_str());
                }
                _ => {}
            },
        }
        res
    }
}

pub enum UnaryOp {
    Neg,
    Not,
}

impl Dump for UnaryOp {
    fn dump(&self) -> String {
        match self {
            Self::Neg => "sub 0 , ".to_string(),
            Self::Not => "eq 0 , ".to_string(),
        }
    }
}

pub enum PrimaryExp {
    ParenExp(Box<Exp>),
    Number(i32),
}

impl Dump for PrimaryExp {
    fn dump(&self) -> String {
        match self {
            Self::ParenExp(e) => e.dump(),
            Self::Number(n) => format!("  %{}", n),
        }
    }
}
