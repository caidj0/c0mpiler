use c0mpiler::{
    ast::{expr::Expr, stmt::Stmt, Visitable},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###" let a114514: str = "Hello world!"; "###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let stmt = Stmt::eat(&mut iter);
    if iter.next().is_some() {
        println!("Invalid!");
    } else {
        println!("{stmt:#?}");
    }
}
