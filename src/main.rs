use c0mpiler::{
    ast::Lit,
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###""Hello world!\n 123214"aaa"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let lit = Lit::visit(&mut iter);

    println!("{lit:?}");
}
