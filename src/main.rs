use std::fs;

use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::SemanticAnalyzer,
};

fn main() {
    let test_str = r##"
fn main() {
    println(
        r#"Why is there no exit in main?
Look in my eyes!
Tell me!
WHY!"#,
    );
}
                    "##
    .to_string();
    let test_str = fs::read_to_string("RCompiler-Testcases/semantic-1/return3/return3.rx").unwrap();
    let lexer = Lexer::new(test_str.as_str());
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter);
    match krate {
        Ok(ast) => {
            // println!("{ast:#?}");
            let (analyzer, result) = SemanticAnalyzer::visit(&ast);
            match result {
                Ok(_) => println!("Semantic check passed!"),
                Err(err) => {
                    println!(
                        "Error occured: {:#?}, analyze stage: {:?}, state: {:?}.",
                        err,
                        analyzer.get_stage(),
                        analyzer.get_state()
                    );
                    println!(
                        "{:#?}",
                        test_str
                            .lines()
                            .nth(analyzer.get_state().current_span.begin.line)
                            .unwrap()
                    );
                }
            }
        }
        Err(err) => {
            println!("{err:#?}");
            println!("{:#?}", test_str.lines().nth(err.pos.line).unwrap());
        }
    }
}
