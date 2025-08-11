use c0mpiler::{
    ast::{Crate, Visitable},
    lexer::{Lexer, TokenBuffer},
};

fn main() {
    let test_str = r###"fn main() {
    let a = 10;
    let b = 20;
    let operator = pick_operator(a);
    let result = match operator {
        Op::Add => compute(a, b, Op::Add),
        Op::Sub => compute(a, b, Op::Sub),
    };
    let doubled = result * 2;
    let mut acc = 0;
    for i in 0..doubled {
        acc += i;
    }
    let _final_score = acc / 3;
}"###;
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
