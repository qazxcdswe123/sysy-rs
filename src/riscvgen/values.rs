use std::fs::File;
use std::io::Result;

use crate::riscvgen::builder::AsmBuilder;

use super::func::Slot;

pub enum AsmValue<'i> {
    Global(&'i str),
    Local(Slot),
    Const(i32),
    Arg(usize),
    Void,
}

macro_rules! asm_value {
    ($info:expr, $v:expr) => {
        AsmValue::from(curr_func!($info).slot_offset($v))
    };
}
pub(crate) use asm_value;

impl<'i> AsmValue<'i> {
    pub fn is_ptr(&self) -> bool {
        matches!(self, Self::Local(slot) if slot.is_ptr)
    }

    /// Write the value to the register
    pub fn write_to(&self, f: &mut File, reg: &'static str) -> Result<()> {
        let mut builder = AsmBuilder::new(f, reg);
        match self {
            AsmValue::Global(symbol) => {
                builder.la(reg, symbol)?;
                builder.lw(reg, reg, 0)
            }
            AsmValue::Local(slot) => builder.lw(&reg, "sp", slot.offset as i32),
            AsmValue::Const(num) => builder.li(&reg, *num),
            _ => unreachable!(),
        }
    }

    /// Write the address of the value to the register
    pub fn write_addr_to(&self, f: &mut File, reg: &'static str) -> Result<()> {
        let mut builder = AsmBuilder::new(f, reg);
        match self {
            AsmValue::Global(symbol) => builder.la(reg, symbol),
            AsmValue::Local(slot) => builder.addi(reg, "sp", slot.offset as i32),
            _ => unreachable!(),
        }
    }

    /// Write the argument to the register
    pub fn write_arg_to(&self, f: &mut File, reg: &'static str, sp_offset: usize) -> Result<()> {
        let mut builder = AsmBuilder::new(f, reg);
        match self {
            AsmValue::Arg(idx) => {
                if *idx < 8 {
                    // a0-a7
                    builder.mv(reg, &format!("a{}", *idx))
                } else {
                    // stack
                    builder.lw(reg, "sp", (sp_offset + (*idx - 8) * 4) as i32)
                }
            }
            _ => unreachable!(),
        }
    }

    /// Read the value from the register
    pub fn read_from(&self, f: &mut File, reg: &'static str, tmp: &'static str) -> Result<()> {
        let mut builder = AsmBuilder::new(f, reg);
        match self {
            AsmValue::Global(symbol) => {
                builder.la(tmp, symbol)?;
                builder.sw(reg, tmp, 0)
            }
            AsmValue::Local(slot) => builder.sw(&reg, "sp", slot.offset as i32),
            AsmValue::Const(_) => unreachable!(),
            AsmValue::Arg(idx) => {
                if *idx < 8 {
                    builder.mv(reg, &format!("a{}", *idx))
                } else {
                    builder.sw(reg, "sp", ((*idx - 8) * 4) as i32)
                }
            }
            AsmValue::Void => Ok(()),
        }
    }
}

impl<'i> From<LocalValue> for AsmValue<'i> {
    fn from(value: LocalValue) -> Self {
        match value {
            LocalValue::Local(slot) => Self::Local(slot),
            LocalValue::Const(num) => Self::Const(num),
        }
    }
}

impl<'i> From<Option<Slot>> for AsmValue<'i> {
    fn from(value: Option<Slot>) -> Self {
        match value {
            Some(slot) => Self::Local(slot),
            None => Self::Void,
        }
    }
}

/// Local value, simplified version of AsmValue
pub enum LocalValue {
    Local(Slot),
    Const(i32),
}

impl<'i> From<AsmValue<'i>> for LocalValue {
    fn from(value: AsmValue) -> Self {
        match value {
            AsmValue::Local(slot) => Self::Local(slot),
            AsmValue::Const(num) => Self::Const(num),
            _ => unreachable!(),
        }
    }
}
