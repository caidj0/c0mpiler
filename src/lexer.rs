use fancy_regex::Regex;

use crate::tokens::{TokenType, get_all_tokens};

// 传入一个字符流，不断获取下一个 token
pub struct Lexer {
    token_types: Vec<(TokenType, Regex)>,
}

impl Lexer {
    pub fn new() -> Lexer {
        let tokens = get_all_tokens();

        let token_types = tokens
            .iter()
            .map(|(t, s)| (t.clone(), Regex::new(s).unwrap()))
            .collect();

        Lexer { token_types }
    }

    pub fn next_token<'a>(&self, s: &'a str) -> (&'a str, Option<(TokenType, &'a str)>) {
        let ret = self
            .token_types
            .iter()
            .filter_map(|(t, re)| {
                re.find(s)
                    .unwrap()
                    .map(|result| (t.clone(), result.as_str()))
            })
            .reduce(|a, b| if a.1.len() < b.1.len() { b } else { a });

        if let Some(r) = &ret {
            if r.0.should_skip() {
                self.next_token(s.split_at(r.1.len()).1)
            } else {
                (s.split_at(r.1.len()).1, ret)
            }
        } else {
            (s, None)
        }
    }
}
