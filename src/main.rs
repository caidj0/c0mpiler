use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###"
fn main() {
    1 + 1
    abcde
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter);
    match krate {
        Ok(ast) => {
            println!("{ast:#?}");
            // let mut semantic = SemanticAnalyzer::new();
            // let result = semantic.visit(&ast);
            // match result {
            //     Ok(_) => println!("{:#?}", semantic),
            //     Err(err) => println!("{:#?}", err),
            // }
        }
        Err(err) => {
            println!("{err:#?}");
            println!("{:#?}", test_str.lines().nth(err.pos.line).unwrap());
        }
    }
}
