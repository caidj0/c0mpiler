use c0mpiler::{
    lexer::Lexer,
    tokens::TokenType,
};

fn main() {
    // let mut test_str = r########################################################################################################################################"r####################################################################################################################################"asdsa"####################################################################################################################################dsdss"########################################################################################################################################;

    let test_str = r#####"let a = 0b02"#####;
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
