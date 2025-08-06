use c0mpiler::{
    lexer::Lexer,
    tokens::{TokenType},
};

fn main() {
    // let mut test_str = r########################################################################################################################################"r####################################################################################################################################"asdsa"####################################################################################################################################dsdss"########################################################################################################################################;

    let test_str = r#####"use c0mpiler::{
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
    let mut lexer = Lexer::new(test_str);

    loop {
        let lexer_result = lexer.next_token();
        match lexer_result.token {
            Ok(token) => {
                if token.0 == TokenType::EOF {
                    break;
                }
                println!("{:?} at line {} col {}", token, lexer_result.line, lexer_result.col);
            }
            Err(info) => {
                panic!(
                    "{} at line {} col {}",
                    info, lexer_result.line, lexer_result.col
                );
            }
        }
    }
}
