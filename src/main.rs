use c0mpiler::{
    ast::{Visitable, stmt::Stmt},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###" let a114514 = [1, 2, 3]; "###;
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
