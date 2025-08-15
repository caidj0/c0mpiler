use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::SemanticAnalyzer,
};

fn main() {
    let test_str = r###"
struct A{}
struct B{}

trait C{}

fn main() {
    match a + b {}
    if a && b {}
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let cra = Crate::eat(&mut iter);
    match cra {
        Ok(ast) => {
            println!("{ast:#?}");
            let mut analyzer = SemanticAnalyzer::new();
            match analyzer.parse(&ast) {
                Ok(()) => {
                    println!("{:?}", analyzer.type_names);
                    println!("{:?}", analyzer.value_names);
                },
                Err(err) => {
                    println!("{:?}", err);
                }
            }
        }
        Err(err) => {
            println!("{err:#?}");
            println!("{:#?}", test_str.lines().nth(err.pos.line).unwrap());
        }
    }
}
