use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::{Result, Write};

use ast::Dump;

mod ast;
mod riscv;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let koopa = ast.dump();

    match mode.as_str() {
        "-koopa" => {
            let mut outfile = std::fs::File::create(output)?;
            writeln!(outfile, "{}", koopa)?;
        }
        "-riscv" => {
            let asm = riscv::koopa2riscv(koopa).unwrap();
            let mut outfile = std::fs::File::create(output)?;
            writeln!(outfile, "{}", asm)?;
        }
        _ => {
            panic!("Invalid mode");
        }
    }

    Ok(())
}
