use std::fs;

use c0mpiler::{
    ast::{Crate, Eatable},
    irgen::IRGenerator,
    lexer::{Lexer, TokenBuffer},
    semantics::analyzer::SemanticAnalyzer,
    utils::test,
};

fn main() {
    let test_str =
        fs::read_to_string("RCompiler-Testcases/IR-1/src/comprehensive1/comprehensive1.rx").unwrap();

    let lexer = Lexer::new(&test_str);
    let buffer = TokenBuffer::new(lexer).unwrap();
    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter).unwrap();
    let (analyzer, result) = SemanticAnalyzer::visit(&krate);
    result.unwrap();

    let mut generator = IRGenerator::new(&analyzer);
    generator.visit(&krate);
    let ir = generator.print();
    println!("{ir}");
}
