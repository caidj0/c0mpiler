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

fn main() {
    print("Hello world!");
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let cra = Crate::eat(&mut iter);
    println!("{:#?}", cra);
}
