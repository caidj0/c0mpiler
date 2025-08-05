use c0mpiler::lexer::Lexer;

fn main() {
    let lexer = Lexer::new();
    let mut test_str = r"== 123;";

    while !test_str.is_empty() {
        println!("{}", test_str);
        let (new_test_str, res) = lexer.next_token(test_str);
        test_str = new_test_str;
        println!("{:?}", res.unwrap());
    }
}
