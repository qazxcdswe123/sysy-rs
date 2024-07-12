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

pub struct Stmt {
    pub num: i32,
}

impl Dump for Stmt {
    fn dump(&self) -> String {
        format!("  ret {}", self.num)
    }
}
