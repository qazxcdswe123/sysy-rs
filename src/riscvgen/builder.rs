use super::func::FunctionContext;
use std::{
    fs::File,
    io::{Result, Write},
};

pub struct AsmBuilder<'f> {
    f: &'f mut File,
    tmp: &'static str,
}

impl<'f> AsmBuilder<'f> {
    pub fn new(f: &'f mut File, tmp: &'static str) -> Self {
        Self { f, tmp }
    }

    pub fn li(&mut self, rd: &str, imm: i32) -> Result<()> {
        writeln!(self.f, "  li {rd}, {imm}")
    }

    pub fn la(&mut self, rd: &str, label: &str) -> Result<()> {
        writeln!(self.f, "  la {rd}, {label}")
    }

    pub fn mv(&mut self, rd: &str, rs: &str) -> Result<()> {
        if rd != rs {
            writeln!(self.f, "  mv {rd}, {rs}")
        } else {
            Ok(())
        }
    }

    pub fn op1(&mut self, op: &str, rd: &str, src: &str) -> Result<()> {
        writeln!(self.f, "  {op} {rd}, {src}")
    }

    pub fn op2(&mut self, op: &str, rd: &str, lhs: &str, rhs: &str) -> Result<()> {
        writeln!(self.f, "  {op} {rd}, {lhs}, {rhs}")
    }

    pub fn addi(&mut self, rd: &str, rs: &str, imm: i32) -> Result<()> {
        if (-2048..=2047).contains(&imm) {
            writeln!(self.f, "  addi {rd}, {rs}, {imm}")
        } else {
            self.li(self.tmp, imm)?;
            self.op2("add", rd, rs, self.tmp)
        }
    }

    pub fn slli(&mut self, rd: &str, rs: &str, imm: i32) -> Result<()> {
        writeln!(self.f, "  slli {rd}, {rs}, {imm}")
    }

    pub fn muli(&mut self, rd: &str, rs: &str, imm: i32) -> Result<()> {
        if imm == 0 {
            self.mv(rd, "x0")
        } else if imm > 0 && (imm & (imm - 1) == 0) {
            // imm is a power of 2
            self.slli(rd, rs, imm.trailing_zeros() as i32)
        } else {
            self.li(&self.tmp, imm)?;
            self.op2("mul", rd, rs, self.tmp)
        }
    }

    pub fn sw(&mut self, rs: &str, addr: &str, offset: i32) -> Result<()> {
        if (-2048..=2047).contains(&offset) {
            writeln!(self.f, "  sw {rs}, {offset}({addr})")
        } else {
            self.addi(self.tmp, addr, offset)?;
            let tmp = self.tmp;
            writeln!(self.f, "  sw {rs}, 0({tmp})")
        }
    }

    pub fn lw(&mut self, rd: &str, addr: &str, offset: i32) -> Result<()> {
        if (-2048..=2047).contains(&offset) {
            writeln!(self.f, "  lw {rd}, {offset}({addr})")
        } else {
            self.addi(self.tmp, addr, offset)?;
            let tmp = self.tmp;
            writeln!(self.f, "  lw {rd}, 0({tmp})")
        }
    }

    pub fn bnez(&mut self, cond: &str, label: &str) -> Result<()> {
        writeln!(self.f, "  bnez {cond}, {label}")
    }

    pub fn j(&mut self, label: &str) -> Result<()> {
        writeln!(self.f, "  j {label}")
    }

    pub fn call(&mut self, func: &str) -> Result<()> {
        writeln!(self.f, "  call {func}")
    }

    pub fn prologue(&mut self, func_name: &str, context: &FunctionContext) -> Result<()> {
        writeln!(self.f, "  .text")?;
        writeln!(self.f, "  .globl {}", &func_name[1..])?;
        writeln!(self.f, "{}:", &func_name[1..])?;

        let offset = context.sp_offset() as i32;
        if offset != 0 {
            self.addi("sp", "sp", -offset)?;
            if !context.is_leaf() {
                self.sw("ra", "sp", offset - 4)?;
            }
        }

        Ok(())
    }

    pub fn epilogue(&mut self, context: &FunctionContext) -> Result<()> {
        let offset = context.sp_offset() as i32;
        if offset != 0 {
            if !context.is_leaf() {
                self.lw("ra", "sp", offset - 4)?;
            }
            self.addi("sp", "sp", offset)?;
        }

        writeln!(self.f, "  ret")
    }
}
