mod builder;
mod func;
mod gen;
mod values;

use func::FunctionContext;
use koopa::ir::{Program, Type, Value};
use r#gen::GenerateAsm;
use std::{collections::HashMap, fs::File, io::Result};

pub fn generate_asm(program: &Program, path: &str) -> Result<()> {
    Type::set_ptr_size(4);
    program.generate(&mut File::create(path)?, &mut ProgramContext::new(program))
}

/// Returns a reference to the current function information.
macro_rules! curr_func {
    ($info:expr) => {
        $info.curr_func().unwrap()
    };
}
pub(crate) use curr_func;

/// Returns a mutable reference to the current function information.
macro_rules! curr_func_mut {
    ($info:expr) => {
        $info.curr_func_mut().unwrap()
    };
}
pub(crate) use curr_func_mut;

pub struct ProgramContext<'a> {
    program: &'a Program,
    values: HashMap<Value, String>,
    curr_func: Option<FunctionContext>,
}

impl<'p> ProgramContext<'p> {
    pub fn new(program: &'p Program) -> Self {
        Self {
            program,
            values: HashMap::new(),
            curr_func: None,
        }
    }

    pub fn program(&self) -> &'p Program {
        self.program
    }

    pub fn value(&self, value: Value) -> &str {
        self.values.get(&value).unwrap()
    }

    pub fn insert_value(&mut self, value: Value, name: String) {
        self.values.insert(value, name);
    }

    pub fn curr_func(&self) -> Option<&FunctionContext> {
        self.curr_func.as_ref()
    }

    pub fn curr_func_mut(&mut self) -> Option<&mut FunctionContext> {
        self.curr_func.as_mut()
    }

    pub fn set_curr_func(&mut self, func: FunctionContext) {
        self.curr_func = Some(func);
    }
}
