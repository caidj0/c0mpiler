use c0mpiler::{
    ast::{Visitable, item::Item},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###"fn main() {
    let numbers: [i32; 3] = [10, 20, 30];
}"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let stmt = Item::eat(&mut iter);
    if iter.next().is_some() {
        println!("Invalid!");
    } else {
        println!("{stmt:#?}");
    }
}
