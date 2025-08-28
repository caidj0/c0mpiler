use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::SemanticAnalyzer,
};

fn main() {
    let test_str = r###"
fn main() {
    1;
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter);
    match krate {
        Ok(ast) => {
            // println!("{ast:#?}");
            let mut semantic = SemanticAnalyzer::new();
            let result = semantic.visit(&ast);
            match result {
                Ok(_) => println!("Semantic check passed!"),
                Err(err) => {
                    println!(
                        "Error occured: {:#?}, analyze stage: {:?}, state: {:?}.",
                        err,
                        semantic.get_stage(),
                        semantic.get_state()
                    );
                    println!(
                        "{:#?}",
                        test_str
                            .lines()
                            .nth(semantic.get_state().current_span.begin.line)
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
