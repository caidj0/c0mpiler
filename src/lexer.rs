use std::fmt::Display;

use fancy_regex::Regex;

use crate::{
    ast::{BinOp, Lit, LitKind, UnOp},
    tokens::{TokenType, get_all_tokens},
    utils::string::{parse_number_literal, parse_quoted_content},
};

#[derive(Debug)]
pub struct TokenPosition {
    line: usize,
    col: usize,
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

impl<'a> TryInto<Lit> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<Lit, Self::Error> {
        match self.token_type {
            TokenType::True | TokenType::False => Ok(Lit {
                kind: LitKind::Bool,
                symbol: self.lexeme.to_owned(),
                suffix: None,
            }),

            TokenType::Byte => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\'').ok_or(())?;
                Ok(Lit {
                    kind: LitKind::Byte,
                    symbol,
                    suffix,
                })
            }

            TokenType::Character => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\'').ok_or(())?;
                Ok(Lit {
                    kind: LitKind::Char,
                    symbol,
                    suffix,
                })
            }

            TokenType::Integer => {
                let (symbol, suffix) = parse_number_literal(self.lexeme);
                Ok(Lit {
                    kind: LitKind::Integer,
                    symbol,
                    suffix,
                })
            }

            TokenType::Float => {
                let suffix_pos = self
                    .lexeme
                    .find(|c: char| !(c.is_ascii_digit() || c == '.' || c == '_'));

                let symbol =
                    suffix_pos.map_or(self.lexeme.to_owned(), |pos| self.lexeme[..pos].to_owned());
                let suffix = suffix_pos.map(|pos| self.lexeme[pos..].to_owned());

                Ok(Lit {
                    kind: LitKind::Float,
                    symbol,
                    suffix,
                })
            }

            TokenType::String => {
                let (symbol, suffix) = parse_quoted_content(self.lexeme, '\"').ok_or(())?;
                Ok(Lit {
                    kind: LitKind::Str,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawString => {
                let (sharps, other) = self.lexeme.split_once('\"').ok_or(())?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or(())?;
                let sharp_num = sharps.len() - 1;
                let suffix = &suffix_with_sharps[sharp_num..];
                let suffix = if suffix.is_empty() {
                    None
                } else {
                    Some(suffix.to_owned())
                };

                Ok(Lit {
                    kind: LitKind::StrRaw(sharp_num as u8),
                    symbol: symbol.to_owned(),
                    suffix,
                })
            }

            TokenType::ByteString => {
                let content = &self.lexeme[1..];
                let (symbol, suffix) = parse_quoted_content(content, '\"').ok_or(())?;
                Ok(Lit {
                    kind: LitKind::ByteStr,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawByteString => {
                let without_b = &self.lexeme[1..];
                let (sharps, other) = without_b.split_once('\"').ok_or(())?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or(())?;
                let sharp_num = sharps.len() - 1;
                let suffix = &suffix_with_sharps[sharp_num..];
                let suffix = if suffix.is_empty() {
                    None
                } else {
                    Some(suffix.to_owned())
                };

                Ok(Lit {
                    kind: LitKind::ByteStrRaw(sharp_num as u8),
                    symbol: symbol.to_owned(),
                    suffix,
                })
            }

            TokenType::CString => {
                let content = &self.lexeme[1..];
                let (symbol, suffix) = parse_quoted_content(content, '\"').ok_or(())?;
                Ok(Lit {
                    kind: LitKind::CStr,
                    symbol,
                    suffix,
                })
            }

            TokenType::RawCString => {
                let without_c = &self.lexeme[1..];
                let (sharps, other) = without_c.split_once('\"').ok_or(())?;
                let (symbol, suffix_with_sharps) = other.rsplit_once('\"').ok_or(())?;
                let sharp_num = sharps.len() - 1;
                let suffix = &suffix_with_sharps[sharp_num..];
                let suffix = if suffix.is_empty() {
                    None
                } else {
                    Some(suffix.to_owned())
                };

                Ok(Lit {
                    kind: LitKind::CStrRaw(sharp_num as u8),
                    symbol: symbol.to_owned(),
                    suffix,
                })
            }

            _ => Err(()),
        }
    }
}

impl<'a> TryInto<UnOp> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<UnOp, Self::Error> {
        match self.token_type {
            TokenType::Star => Ok(UnOp::Deref),
            TokenType::Not => Ok(UnOp::Not),
            TokenType::Minus => Ok(UnOp::Neg),
            _ => Err(()),
        }
    }
}

impl<'a> TryInto<BinOp> for &Token<'a> {
    type Error = ();

    fn try_into(self) -> Result<BinOp, Self::Error> {
        match self.token_type {
            TokenType::Plus => Ok(BinOp::Add),
            TokenType::Minus => Ok(BinOp::Sub),
            TokenType::Star => Ok(BinOp::Mul),
            TokenType::Slash => Ok(BinOp::Div),
            TokenType::Percent => Ok(BinOp::Rem),
            TokenType::AndAnd => Ok(BinOp::And),
            TokenType::OrOr => Ok(BinOp::Or),
            TokenType::Caret => Ok(BinOp::BitXor),
            TokenType::And => Ok(BinOp::BitAnd),
            TokenType::Or => Ok(BinOp::BitOr),
            TokenType::Shl => Ok(BinOp::Shl),
            TokenType::Shr => Ok(BinOp::Shr),
            TokenType::EqEq => Ok(BinOp::Eq),
            TokenType::Lt => Ok(BinOp::Lt),
            TokenType::Le => Ok(BinOp::Le),
            TokenType::Ne => Ok(BinOp::Ne),
            TokenType::Ge => Ok(BinOp::Ge),
            TokenType::Gt => Ok(BinOp::Gt),
            _ => Err(()),
        }
    }
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

impl<'a> Iterator for TokenIter<'a> {
    type Item = &'a Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.buffer.len() {
            return None;
        }

        let ret = &self.buffer[self.pos];
        self.pos += 1;
        Some(ret)
    }
}

impl<'a> TokenIter<'a> {
    pub fn update(&mut self, new_iter: Self) {
        self.pos = new_iter.pos;
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
