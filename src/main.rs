use c0mpiler::{
    ast::{Crate, Visitable},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###"impl Dummy {
    fn new() -> Self {
        1 + 1
    }
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let stmt = Crate::eat(&mut iter);
    if iter.next().is_some() {
        println!("Invalid!");
    } else {
        println!("{stmt:#?}");
    }
}
