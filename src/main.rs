use c0mpiler::{
    ast::{Visitable, expr::Expr},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###" !1 + -1 * 1 "###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let expr = Expr::eat(&mut iter);
    if iter.next().is_some() {
        println!("Invalid!");
    } else {
        println!("{expr:#?}");
    }
}
