use c0mpiler::{lexer::Lexer, tokens::TokenType};

fn assert_tokens(code: &str, expected_tokens: &[(TokenType, &str)]) {
    let mut lexer = Lexer::new(code);
    for &(ref expected_type, expected_lexeme) in expected_tokens {
        let token = lexer.next_token().unwrap();
        assert_eq!(token.token_type, expected_type.clone());
        assert_eq!(token.lexeme, expected_lexeme);
    }
    assert_eq!(lexer.next_token().unwrap().token_type, TokenType::EOF);
}

#[test]
fn test_simple_tokens() {
    let code = "let x = 10;";
    let expected = vec![
        (TokenType::Let, "let"),
        (TokenType::Id, "x"),
        (TokenType::Eq, "="),
        (TokenType::Integer, "10"),
        (TokenType::Semi, ";"),
    ];
    assert_tokens(code, &expected);
}

#[test]
fn test_operators() {
    let code = "+ - * / % = == != < > <= >= && || !";
    let expected = vec![
        (TokenType::Plus, "+"),
        (TokenType::Minus, "-"),
        (TokenType::Star, "*"),
        (TokenType::Slash, "/"),
        (TokenType::Percent, "%"),
        (TokenType::Eq, "="),
        (TokenType::EqEq, "=="),
        (TokenType::Ne, "!="),
        (TokenType::Lt, "<"),
        (TokenType::Gt, ">"),
        (TokenType::Le, "<="),
        (TokenType::Ge, ">="),
        (TokenType::AndAnd, "&&"),
        (TokenType::OrOr, "||"),
        (TokenType::Not, "!"),
    ];
    assert_tokens(code, &expected);
}

#[test]
fn test_keywords_and_literals() {
    let code = "let fn true false 123 \"test\"";
    let expected = vec![
        (TokenType::Let, "let"),
        (TokenType::Fn, "fn"),
        (TokenType::True, "true"),
        (TokenType::False, "false"),
        (TokenType::Integer, "123"),
        (TokenType::String, "\"test\""),
    ];
    assert_tokens(code, &expected);
}

#[test]
fn test_with_comments_and_whitespace() {
    let code = r#"
            // This is a comment
            let x = 5; // another comment
            /* 
                This is a 
                block comment 
            */
            let y = 10;
        "#;
    let expected = vec![
        (TokenType::Let, "let"),
        (TokenType::Id, "x"),
        (TokenType::Eq, "="),
        (TokenType::Integer, "5"),
        (TokenType::Semi, ";"),
        (TokenType::Let, "let"),
        (TokenType::Id, "y"),
        (TokenType::Eq, "="),
        (TokenType::Integer, "10"),
        (TokenType::Semi, ";"),
    ];
    assert_tokens(code, &expected);
}

#[test]
fn test_all_literals() {
    let code = r####"
            // Integers
            123 0xff 0o77 0b1101 1_000_000
            // Floats
            123.456 1. 0.5
            // Characters
            'a' '\n' '\\'
            // Strings
            "hello world" "hello\nworld"
            // Raw Strings
            r#"hello "world""# r##"raw string with ##"##
            "string
literal"
            "literal with \
             escaped newline"
        "####;
    let expected = vec![
        // Integers
        (TokenType::Integer, "123"),
        (TokenType::Integer, "0xff"),
        (TokenType::Integer, "0o77"),
        (TokenType::Integer, "0b1101"),
        (TokenType::Integer, "1_000_000"),
        // Floats
        (TokenType::Float, "123.456"),
        (TokenType::Float, "1."),
        (TokenType::Float, "0.5"),
        // Characters
        (TokenType::Character, "'a'"),
        (TokenType::Character, "'\\n'"),
        (TokenType::Character, "'\\\\'"),
        // Strings
        (TokenType::String, "\"hello world\""),
        (TokenType::String, "\"hello\\nworld\""),
        // Raw Strings
        (TokenType::RawString, "r#\"hello \"world\"\"#"),
        (TokenType::RawString, "r##\"raw string with ##\"##"),
        (TokenType::String, "\"string\nliteral\""),
        (
            TokenType::String,
            "\"literal with \\\n             escaped newline\"",
        ),
    ];
    assert_tokens(code, &expected);
}

#[test]
fn test_lex_bad_binary_literal() {
    let cases = [
        "0b121",
        "0b10_10301",
        "0b30",
        "0b41",
        "0b5",
        "0b6",
        "0b7",
        "0b8",
        "0b9",
        "0x",
        "0xu32",
        "0ou32",
        "0bu32",
        "0b",
        "0o18",
    ];
    for code in cases.iter() {
        let mut lexer = Lexer::new(code);
        assert!(lexer.next_token().is_err(), "should error for {code}");
    }
}

#[test]
fn test_lex_bad_str_literal_as_char() {
    let cases = ["'1 + 1", "'abc'", "''", r"'\x10\x10'", r"'\x1'"];
    for code in cases.iter() {
        let mut lexer = Lexer::new(code);
        assert!(lexer.next_token().is_err(), "should error for {code}");
    }
}

#[test]
fn test_unterminated_comment() {
    let code = "/*";
    let mut lexer = Lexer::new(code);
    assert!(lexer.next_token().is_err());
}

#[test]
fn test_lex_stray_backslash() {
    let code = "\\";
    let mut lexer = Lexer::new(code);
    assert!(lexer.next_token().is_err());
}
