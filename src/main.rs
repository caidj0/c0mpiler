use c0mpiler::{
    ast::{Expr, Visitable},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###" - - - 123 "###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let expr = Expr::visit(&mut iter);

    println!("{expr:#?}");
}
