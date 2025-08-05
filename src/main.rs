use c0mpiler::{
    lexer::Lexer,
    tokens::{TokenType, get_all_tokens},
};

fn main() {
    let lexer = Lexer::new();
    let mut test_str = r####"r##"asdsa"##dsdss"####;

    for x in get_all_tokens() {
        if x.0 == TokenType::RawString {
            println!("{}", x.1);
        }
    }

    while !test_str.is_empty() {
        println!("{}", test_str);
        let (new_str, result) = lexer.next_token(test_str);
        test_str = new_str;
        println!("{:?}", result.unwrap());
    }
}
