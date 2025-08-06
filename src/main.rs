use c0mpiler::{
    lexer::{Lexer, TokenStream},
    tokens::TokenType,
};

fn main() {
    // let mut test_str = r########################################################################################################################################"r####################################################################################################################################"asdsa"####################################################################################################################################dsdss"########################################################################################################################################;

    let test_str = r#####"let a = "Hello world!";"#####;
    let lexer = Lexer::new(test_str);
    let mut stream = TokenStream::new(lexer);

    let mut test_count = 5;

    loop {
        let token = stream.next_token();

        if token.token_type == TokenType::EOF {
            break;
        }
        println!("{:?}", token);

        if token.token_type == TokenType::String && test_count > 0 {
            test_count -= 1;
            stream.go_back();
        }
    }
}
