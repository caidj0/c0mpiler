use std::fmt::Display;

use fancy_regex::Regex;

use crate::{
    ast::{ASTError, ASTResult},
    tokens::{TokenType, get_all_tokens},
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TokenPosition {
    pub line: usize,
    pub col: usize,
}

impl Display for TokenPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {} col {}", self.line, self.col)
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub pos: TokenPosition,
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

    pub fn get_pos(&self) -> TokenPosition {
        TokenPosition {
            line: self.line,
            col: self.col,
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>, (String, TokenPosition)> {
        if self.codes.is_empty() {
            return Ok(Token {
                token_type: TokenType::EOF,
                lexeme: self.codes,
                pos: self.get_pos(),
            });
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

        let pos = self.get_pos();
        if let Some(r) = ret {
            for c in r.1.chars() {
                if c == '\n' {
                    self.line += 1;
                    self.col = 0
                } else {
                    self.col += 1;
                }
            }
            self.codes = self.codes.split_at(r.1.len()).1;

            if r.0.should_err() {
                return Err((
                    format!(
                        "The token \n \t {} \n was deliberately marked as an error",
                        r.1
                    ),
                    self.get_pos(),
                ));
            }

            if r.0.should_skip() {
                self.next_token()
            } else {
                Ok(Token {
                    token_type: r.0,
                    lexeme: r.1,
                    pos,
                })
            }
        } else {
            Err(("Can't match any tokens!".to_owned(), pos))
        }
    }
}

pub struct TokenBuffer<'a> {
    buffer: Vec<Token<'a>>,
}

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    buffer: &'a Vec<Token<'a>>,
    pos: usize,
}

impl<'a> TokenIter<'a> {
    pub fn update(&mut self, new_iter: Self) {
        self.pos = new_iter.pos;
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> ASTResult<&'a Token<'a>> {
        if self.pos >= self.buffer.len() {
            return Err(ASTError {
                kind: crate::ast::ASTErrorKind::EOF,
                pos: self.get_last_pos(),
            });
        }

        let ret = &self.buffer[self.pos];
        self.pos += 1;
        Ok(ret)
    }

    pub fn peek(&self) -> ASTResult<&'a Token<'a>> {
        if self.pos >= self.buffer.len() {
            return Err(ASTError {
                kind: crate::ast::ASTErrorKind::EOF,
                pos: self.get_last_pos(),
            });
        }
        Ok(&self.buffer[self.pos])
    }

    pub fn get_last_pos(&self) -> TokenPosition {
        self.buffer
            .last()
            .map_or(TokenPosition { line: 0, col: 0 }, |x| x.pos.clone())
    }
}

impl<'a> TokenBuffer<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> TokenBuffer<'a> {
        let mut buffer = Vec::new();

        loop {
            let result = lexer.next_token();
            match result {
                Ok(token) => {
                    if token.token_type == TokenType::EOF {
                        break;
                    }
                    buffer.push(token);
                }
                Err(info) => {
                    panic!("Tokenlize error: {} at {}", info.0, info.1);
                }
            }
        }

        TokenBuffer { buffer }
    }

    pub fn iter(&self) -> TokenIter {
        TokenIter {
            buffer: &self.buffer,
            pos: 0,
        }
    }
}

pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    buffer: Vec<Token<'a>>,
    back_count: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(lexer: Lexer<'a>) -> TokenStream<'a> {
        TokenStream {
            lexer,
            buffer: Vec::new(),
            back_count: 0,
        }
    }

    pub fn next_token(&mut self) -> &Token<'a> {
        if self.back_count == 0 {
            let result = self.lexer.next_token();
            match result {
                Ok(token) => {
                    self.buffer.push(token);
                    self.buffer.last().unwrap()
                }
                Err(info) => {
                    panic!("{} at {}", info.0, info.1);
                }
            }
        } else {
            self.back_count -= 1;
            &self.buffer[self.buffer.len() - 1 - self.back_count]
        }
    }

    pub fn go_back(&mut self) {
        self.back_count += 1;
    }
}
