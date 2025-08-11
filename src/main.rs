use c0mpiler::{
    ast::{Visitable, item::Item},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###"fn main(){
    let result = match operator {
        Op::Add => compute(a, b, Op::Add),
        Op::Sub => compute(a, b, Op::Sub),
    };
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
