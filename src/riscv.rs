use koopa::{front::Driver, ir::*};

trait GenerateASM {
    fn generate_asm(&self) -> Result<Vec<String>, String>;
}

impl GenerateASM for Program {
    fn generate_asm(&self) -> Result<Vec<String>, String> {
        let mut asm = Vec::<String>::new();
        asm.push("    .text".to_string());
        for &func in self.func_layout() {
            asm.extend(self.func(func).generate_asm()?)
        }
        Ok(asm)
    }
}

impl GenerateASM for FunctionData {
    fn generate_asm(&self) -> Result<Vec<String>, String> {
        let mut asm = Vec::<String>::new();
        asm.push(format!("    .globl {}", &self.name()[1..]));
        asm.push(format!("{}:", &self.name()[1..]));
        for (&_bb, node) in self.layout().bbs() {
            for &value in node.insts().keys() {
                //
                let value_data = self.dfg().value(value);
                match value_data.kind() {
                    ValueKind::Return(ret_inst) => {
                        match ret_inst.value() {
                            Some(ret_val) => {
                                let ret_data = self.dfg().value(ret_val);
                                match ret_data.kind() {
                                    ValueKind::Integer(int) => {
                                        asm.push(format!("    li a0, {}", int.value()));
                                    }
                                    _ => unimplemented!(),
                                }
                            }
                            None => {}
                        }
                        asm.push("    ret".to_string());
                    }
                    kind => return Err(format!("{:?}", kind)),
                }
            }
        }
        Ok(asm)
    }
}

pub fn koopa2riscv(koopa: String) -> Result<String, String> {
    let driver = Driver::from(koopa);
    let program = driver.generate_program().unwrap();

    let riscv = program.generate_asm()?;

    Ok(riscv.join("\n"))
}
