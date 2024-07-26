use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;

mod ast;
mod irgen;
mod riscvgen;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().expect("No mode specified");
    let input = args.next().expect("No input file specified");
    args.next();
    let output = args.next().expect("No output file specified");

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    let koopa = irgen::generate_ir(&ast).expect("IR build error");

    match mode.as_str() {
        "-koopa" => {
            let mut text_generator = KoopaGenerator::new(vec![]);
            text_generator.generate_on(&koopa).unwrap();
            std::fs::write(output, text_generator.writer())?;
        }
        "-riscv" => {
            let mut output_file = std::fs::File::create(output)?;
            riscvgen::generate_assembly(&koopa, &mut output_file)?;
        }
        _ => {
            panic!("Invalid mode");
        }
    }

    Ok(())
}
