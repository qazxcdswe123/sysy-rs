use std::fs::File;
use std::io::Result;
use std::io::Write;

use koopa::ir::entities::ValueData;
use koopa::ir::values::*;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, TypeKind, Value, ValueKind};

use crate::riscvgen::curr_func;
use crate::riscvgen::curr_func_mut;

use super::builder::AsmBuilder;
use super::func::FunctionContext;
use super::values::asm_value;
use super::values::AsmValue;
use super::values::LocalValue;
use super::ProgramContext; // Add this line to import the GenerateAsm trait

pub trait GenerateAsm<'p, 'c> {
    type Out;

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out>;
}

trait GenerateValueToAsm<'p, 'c> {
    type Out;

    fn generate(
        &self,
        f: &mut File,
        context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out>;
}

impl<'p, 'c> GenerateAsm<'p, 'c> for Program {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        // global allocations
        for &value in self.inst_layout() {
            let data = self.borrow_value(value);
            let name = &data.name().as_ref().unwrap()[1..];
            context.insert_value(value, name.to_owned());
            writeln!(f, "  .data")?;
            writeln!(f, "  .globl {name}")?;
            writeln!(f, "{name}:")?;
            data.generate(f, context)?;
            writeln!(f)?;
        }
        // functions
        for &func in self.func_layout() {
            context.set_curr_func(FunctionContext::new(func));
            self.func(func).generate(f, context)?;
        }
        Ok(())
    }
}

impl<'p, 'c> GenerateAsm<'p, 'c> for Function {
    type Out = &'p str;

    fn generate(&self, _f: &mut File, context: &mut ProgramContext<'p>) -> Result<Self::Out> {
        Ok(&context.program().func(*self).name()[1..])
    }
}

// functiondata
impl<'p, 'c> GenerateAsm<'p, 'c> for FunctionData {
    type Out = ();

    fn generate(&self, f: &mut File, context: &mut ProgramContext) -> Result<Self::Out> {
        // skip declaration
        if self.layout().entry_bb().is_none() {
            return Ok(());
        }

        // allocate stack slots and log argument number
        let func = curr_func_mut!(context);
        for value in self.dfg().values().values() {
            // allocate stack slot for local values
            if value.kind().is_local_inst() && !value.used_by().is_empty() {
                func.alloc_slot(value);
            }

            // log argument number
            if let ValueKind::Call(c) = value.kind() {
                func.log_arg_num(c.args().len());
            }
        }

        // generate bb names
        for (&bb, data) in self.dfg().bbs() {
            assert!(data.params().is_empty());
            func.log_bb_name(bb, data.name());
        }

        // prologue
        AsmBuilder::new(f, "t0").prologue(self.name(), func)?;

        // generate inst in bb
        for (bb, node) in self.layout().bbs() {
            let bb_name = bb.generate(f, context)?;
            writeln!(f, "{bb_name}:")?;
            for &inst in node.insts().keys() {
                self.dfg().value(inst).generate(f, context)?;
            }
        }

        writeln!(f)
    }
}

impl<'p, 'i> GenerateAsm<'p, 'i> for BasicBlock {
    type Out = &'i str;

    fn generate(&self, _f: &mut File, context: &'i mut ProgramContext<'p>) -> Result<Self::Out> {
        Ok(curr_func!(context).bb_name(*self))
    }
}

impl<'p, 'c> GenerateAsm<'p, 'c> for Value {
    type Out = AsmValue<'c>;

    fn generate(&self, _: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        if self.is_global() {
            Ok(AsmValue::Global(context.value(*self)))
        } else {
            let func = curr_func!(context);
            let value = context.program().func(func.func()).dfg().value(*self);
            Ok(match value.kind() {
                ValueKind::Integer(i) => AsmValue::Const(i.value()),
                ValueKind::FuncArgRef(r) => AsmValue::Arg(r.index()),
                _ => AsmValue::from(func.slot_offset(value)),
            })
        }
    }
}

impl<'p, 'c> GenerateAsm<'p, 'c> for Integer {
    type Out = ();

    fn generate(&self, f: &mut File, _context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        writeln!(f, "  .word {}", self.value())
    }
}

impl<'p, 'c> GenerateValueToAsm<'p, 'c> for ZeroInit {
    type Out = ();

    fn generate(
        &self,
        f: &mut File,
        _context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out> {
        writeln!(f, "  .zero {}", v.ty().size())
    }
}

impl<'p, 'c> GenerateAsm<'p, 'c> for Aggregate {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        for &value in self.elems() {
            context.program().borrow_value(value).generate(f, context)?;
        }
        Ok(())
    }
}

// load
impl<'p, 'c> GenerateValueToAsm<'p, 'c> for Load {
    type Out = ();

    fn generate(
        &self,
        f: &mut File,
        context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out> {
        let src: AsmValue = self.src().generate(f, context)?;
        src.write_to(f, "t0")?;
        if src.is_ptr() {
            AsmBuilder::new(f, "t1").lw("t0", "t0", 0)?;
        }
        asm_value!(context, v).read_from(f, "t0", "t1")
    }
}

// store
impl<'p, 'c> GenerateAsm<'p, 'c> for Store {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        let sp_offset = curr_func!(context).sp_offset();
        let value = self.value().generate(f, context)?;
        if matches!(value, AsmValue::Arg(_)) {
            value.write_arg_to(f, "t0", sp_offset)?;
        } else {
            value.write_to(f, "t0")?;
        }
        let dest = self.dest().generate(f, context)?;
        if dest.is_ptr() {
            dest.write_to(f, "t1")?;
            AsmBuilder::new(f, "t2").sw("t0", "t1", 0)
        } else {
            dest.read_from(f, "t0", "t1")
        }
    }
}

// getptr
impl<'p, 'c> GenerateValueToAsm<'p, 'c> for GetPtr {
    type Out = ();

    fn generate(
        &self,
        f: &mut File,
        context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out> {
        let src = self.src().generate(f, context)?;
        if src.is_ptr() {
            src.write_to(f, "t0")?;
        } else {
            src.write_addr_to(f, "t0")?;
        }

        self.index().generate(f, context)?.write_to(f, "t1")?;
        let size = match v.ty().kind() {
            TypeKind::Pointer(p) => p.size(),
            _ => unreachable!(),
        };
        let mut builder = AsmBuilder::new(f, "t2");
        builder.muli("t1", "t1", size as i32)?;
        builder.op2("add", "t0", "t0", "t1")?;
        asm_value!(context, v).read_from(f, "t0", "t1")
    }
}

// getelemptr
impl<'p, 'c> GenerateValueToAsm<'p, 'c> for GetElemPtr {
    type Out = ();

    fn generate(
        &self,
        f: &mut File,
        context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out> {
        let src = self.src().generate(f, context)?;
        if src.is_ptr() {
            src.write_to(f, "t0")?;
        } else {
            src.write_addr_to(f, "t0")?;
        }

        self.index().generate(f, context)?.write_to(f, "t1")?;
        let size = match v.ty().kind() {
            TypeKind::Pointer(p) => p.size(),
            _ => unreachable!(),
        };

        let mut builder = AsmBuilder::new(f, "t2");
        builder.muli("t1", "t1", size as i32)?;
        builder.op2("add", "t0", "t0", "t1")?;
        asm_value!(context, v).read_from(f, "t0", "t1")
    }
}

// binary
impl<'p, 'c> GenerateValueToAsm<'p, 'c> for Binary {
    type Out = ();

    fn generate(
        &self,
        f: &mut File,
        context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out> {
        self.lhs().generate(f, context)?.write_to(f, "t0")?;
        self.rhs().generate(f, context)?.write_to(f, "t1")?;
        let mut builder = AsmBuilder::new(f, "t2");

        match self.op() {
            BinaryOp::NotEq => {
                builder.op2("xor", "t0", "t0", "t1")?;
                builder.op1("snez", "t0", "t0")?;
            }
            BinaryOp::Eq => {
                builder.op2("xor", "t0", "t0", "t1")?;
                builder.op1("seqz", "t0", "t0")?;
            }
            BinaryOp::Le => {
                builder.op2("slt", "t0", "t0", "t1")?;
                builder.op1("seqz", "t0", "t0")?;
            }
            BinaryOp::Ge => {
                builder.op2("sgt", "t0", "t0", "t1")?;
                builder.op1("seqz", "t0", "t0")?;
            }
            BinaryOp::Lt => builder.op2("slt", "t0", "t0", "t1")?,
            BinaryOp::Gt => builder.op2("sgt", "t0", "t0", "t1")?,
            BinaryOp::Sar => builder.op2("sra", "t0", "t0", "t1")?,
            BinaryOp::Add => builder.op2("add", "t0", "t0", "t1")?,
            BinaryOp::Sub => builder.op2("sub", "t0", "t0", "t1")?,
            BinaryOp::Mul => builder.op2("mul", "t0", "t0", "t1")?,
            BinaryOp::Div => builder.op2("div", "t0", "t0", "t1")?,
            BinaryOp::Mod => builder.op2("rem", "t0", "t0", "t1")?,
            BinaryOp::And => builder.op2("and", "t0", "t0", "t1")?,
            BinaryOp::Or => builder.op2("or", "t0", "t0", "t1")?,
            BinaryOp::Xor => builder.op2("xor", "t0", "t0", "t1")?,
            BinaryOp::Shl => builder.op2("sll", "t0", "t0", "t1")?,
            BinaryOp::Shr => builder.op2("sra", "t0", "t0", "t1")?,
        }
        asm_value!(context, v).read_from(f, "t0", "t1")
    }
}

// branch
impl<'p, 'c> GenerateAsm<'p, 'c> for Branch {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        self.cond().generate(f, context)?.write_to(f, "t0")?;
        let tlabel = self.true_bb().generate(f, context)?;
        AsmBuilder::new(f, "t1").bnez("t0", tlabel)?;
        let flabel = self.false_bb().generate(f, context)?;
        AsmBuilder::new(f, "t2").j(flabel)
    }
}

// jump
impl<'p, 'c> GenerateAsm<'p, 'c> for Jump {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        let label = self.target().generate(f, context)?;
        AsmBuilder::new(f, "t0").j(label)
    }
}

// call
impl<'p, 'c> GenerateValueToAsm<'p, 'c> for Call {
    type Out = ();

    fn generate(
        &self,
        f: &mut File,
        context: &'c mut ProgramContext<'p>,
        v: &ValueData,
    ) -> Result<Self::Out> {
        let args = self
            .args()
            .iter()
            .map(|v| Ok(v.generate(f, context)?.into()))
            .collect::<Result<Vec<LocalValue>>>()?;
        for (i, arg) in args.into_iter().enumerate() {
            AsmValue::from(arg).write_to(f, "t0")?;
            AsmValue::Arg(i).read_from(f, "t0", "t1")?;
        }
        let callee = self.callee().generate(f, context)?;
        AsmBuilder::new(f, "t0").call(callee)?;
        if !v.used_by().is_empty() {
            asm_value!(context, v).read_from(f, "a0", "t0")
        } else {
            Ok(())
        }
    }
}

// return
impl<'p, 'c> GenerateAsm<'p, 'c> for Return {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        if let Some(value) = self.value() {
            value.generate(f, context)?.write_to(f, "a0")?;
        }
        AsmBuilder::new(f, "t0").epilogue(curr_func!(context))
    }
}

impl<'p, 'c> GenerateAsm<'p, 'c> for ValueData {
    type Out = ();

    fn generate(&self, f: &mut File, context: &'c mut ProgramContext<'p>) -> Result<Self::Out> {
        match self.kind() {
            ValueKind::ZeroInit(i) => i.generate(f, context, self),
            ValueKind::Integer(i) => i.generate(f, context),
            ValueKind::Aggregate(a) => a.generate(f, context),
            ValueKind::Load(l) => l.generate(f, context, self),
            ValueKind::Store(s) => s.generate(f, context),
            ValueKind::GetPtr(g) => g.generate(f, context, self),
            ValueKind::GetElemPtr(g) => g.generate(f, context, self),
            ValueKind::Binary(b) => b.generate(f, context, self),
            ValueKind::Branch(b) => b.generate(f, context),
            ValueKind::Jump(j) => j.generate(f, context),
            ValueKind::Call(c) => c.generate(f, context, self),
            ValueKind::Return(r) => r.generate(f, context),
            _ => Ok(()),
        }
    }
}
