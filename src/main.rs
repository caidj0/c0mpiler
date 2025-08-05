use c0mpiler::{
    lexer::Lexer,
    tokens::{get_all_tokens, TokenType},
};

fn main() {
    let lexer = Lexer::new();
    // let mut test_str = r########################################################################################################################################"r####################################################################################################################################"asdsa"####################################################################################################################################dsdss"########################################################################################################################################;
    let mut test_str = r#####"use c0mpiler::{
    lexer::Lexer,
    tokens::{TokenType, get_all_tokens},
};

fn main() {
    let lexer = Lexer::new();
    for x in get_all_tokens() {
        if x.0 == TokenType::Integer {
            println!("{}", x.1);
        }
    }

    while !test_str.is_empty() {
        // println!("{}", test_str);
        let (new_str, result) = lexer.next_token(test_str);
        test_str = new_str;
        println!("{:?}", result.unwrap());
    }
}
"#####;

    for x in get_all_tokens() {
        if x.0 == TokenType::Integer {
            println!("{}", x.1);
        }
    }

    while !test_str.is_empty() {
        // println!("{}", test_str);
        let (new_str, result) = lexer.next_token(test_str);
        test_str = new_str;
        if let Some(token) = result {
            println!("{:?}", token);
        } else if !test_str.is_empty() {
            panic!();
        }
    }
}
