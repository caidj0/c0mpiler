use c0mpiler::{
    ast::{Crate, Eatable},
    lexer::{Lexer, TokenBuffer},
    semantics::SemanticAnalyzer,
};

fn main() {
    let test_str = r###"
struct A {
    a: i32,
    b: u32,
    c: C,
}
struct B;
struct C;

fn foo(a: A, b: B, c: C) -> String {}

const ABC: u32 = 123;

trait Trait {
    const AAA: u32 = 456;

    fn foo(a:C) {}
}
"###;
    let lexer = Lexer::new(test_str);
    let buffer = TokenBuffer::new(lexer);

    let mut iter = buffer.iter();
    let krate = Crate::eat(&mut iter);
    match krate {
        Ok(ast) => {
            // println!("{ast:#?}");
            let mut semantic = SemanticAnalyzer::new();
            let result = semantic.visit(&ast);
            match result {
                Ok(_) => println!("{:#?}", semantic),
                Err(err) => println!("{:#?}", err),
            }
        }
        Err(err) => {
            println!("{err:#?}");
            println!("{:#?}", test_str.lines().nth(err.pos.line).unwrap());
        }
    }
}
