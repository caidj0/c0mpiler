use c0mpiler::{
    ast::{Crate, Visitable},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###"
fn main() {
    !..b 
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let cra = Crate::eat(&mut iter);
    match cra {
        Ok(ast) => println!("{ast:#?}"),
        Err(err) => {
            println!("{err:#?}");
            println!("{:#?}", test_str.lines().nth(err.pos.line).unwrap());
        }
    }
}
