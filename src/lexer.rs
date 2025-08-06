use fancy_regex::Regex;

use crate::tokens::{TokenType, get_all_tokens};

pub struct LexerResult<'a> {
    pub line: usize,
    pub col: usize,
    pub token: Result<(TokenType, &'a str), String>,
}

pub struct Lexer<'a> {
    token_types: Vec<(TokenType, Regex)>,
    codes: &'a str,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(codes: &'a str) -> Lexer<'a> {
        let tokens = get_all_tokens();

        let token_types = tokens
            .iter()
            .map(|(t, s)| (t.clone(), Regex::new(s).unwrap()))
            .collect();

        Lexer {
            token_types,
            codes,
            line: 0,
            col: 0,
        }
    }

    pub fn next_token(&mut self) -> LexerResult<'a> {
        if self.codes.is_empty() {
            return LexerResult {
                line: self.line,
                col: self.col,
                token: Ok((TokenType::EOF, self.codes)),
            };
        }

        let ret = self
            .token_types
            .iter()
            .filter_map(|(t, re)| {
                re.find(self.codes)
                    .unwrap()
                    .map(|result| (t.clone(), result.as_str()))
            })
            .reduce(|a, b| if a.1.len() < b.1.len() { b } else { a });

        let line = self.line;
        let col = self.col;
        if let Some(r) = &ret {
            for c in r.1.chars() {
                if c == '\n' {
                    self.line += 1;
                    self.col = 0
                } else {
                    self.col += 1;
                }
            }
            self.codes = self.codes.split_at(r.1.len()).1;

            if r.0.should_skip() {
                self.next_token()
            } else {
                LexerResult {
                    line,
                    col,
                    token: Ok(r.clone()),
                }
            }
        } else {
            LexerResult {
                line,
                col,
                token: Err("Can't match any tokens!".to_owned()),
            }
        }
    }
}
