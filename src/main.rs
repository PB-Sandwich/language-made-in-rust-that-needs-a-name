use std::{collections::VecDeque, mem::discriminant};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Keywords
    Let,
    If,
    Else,
    While,
    Fn,

    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    Semicolon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Literals
    Identifier(String),
    Number(String),
    StringLiteral(String),

    // Special
    Eof,
    Any,
    Unknown(char), // For unexpected characters
}

struct Parser<'a> {
    input: &'a str,
    iter: std::str::CharIndices<'a>,
    current_char: Option<(usize, char)>, // (position, current character)
    tokens: Vec<Token>,
    expected_queue: VecDeque<Vec<Token>>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a String) -> Self {
        let mut iter = input.char_indices();
        let current_char = iter.next();
        let tokens = Vec::new();
        let expected_queue = VecDeque::new();
        Parser {
            input,
            iter,
            current_char,
            tokens,
            expected_queue,
        }
    }

    fn advance_char(&mut self) {
        self.current_char = self.iter.next();
    }

    fn advance_char_n(&mut self, n: usize) {
        for _ in 0..n {
            self.current_char = self.iter.next();
        }
    }

    fn expect_one_of(&self, expected: Vec<Token>) -> Result<(), String> {
        if let Some(last_token) = self.tokens.last() {
            if expected.iter().any(|t| t == &Token::Any) {
                return Ok(());
            }

            let last_discriminant = discriminant(last_token);

            if expected
                .iter()
                .any(|t| discriminant(t) == last_discriminant)
            {
                Ok(())
            } else {
                Err(format!(
                    "Expected one of {:?}, found {:?}",
                    expected, last_token
                ))
            }
        } else {
            Err("No token found".to_string())
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char {
                Some((_, c)) if c.is_whitespace() => {
                    self.advance_char();
                }
                _ => break,
            }
        }
    }

    fn parse_token(&mut self) -> Result<Token, String> {
        if let Some((start, c)) = self.current_char {
            let remaining = &self.input[start..];
            println!("{remaining}");

            if remaining.starts_with("let") {
                self.advance_char_n(3);
                Ok(Token::Let)
            } else if c == '=' {
                self.advance_char();
                Ok(Token::Equal)
            } else if c == '+' {
                self.advance_char();
                Ok(Token::Plus)
            } else if c == '-' {
                self.advance_char();
                Ok(Token::Minus)
            } else if c == '*' {
                self.advance_char();
                Ok(Token::Star)
            } else if c == '/' {
                self.advance_char();
                Ok(Token::Slash)
            } else if c == '(' {
                self.advance_char();
                Ok(Token::LParen)
            } else if c == ')' {
                self.advance_char();
                Ok(Token::RParen)
            } else if c == '{' {
                self.advance_char();
                Ok(Token::LBrace)
            } else if c == '}' {
                self.advance_char();
                Ok(Token::RBrace)
            } else if c == ',' {
                self.advance_char();
                Ok(Token::Comma)
            } else if remaining.starts_with("if") {
                self.advance_char_n(2);
                Ok(Token::If)
            } else if remaining.starts_with("else") {
                self.advance_char_n(4);
                Ok(Token::Else)
            } else if remaining.starts_with("while") {
                self.advance_char_n(5);
                Ok(Token::While)
            } else if remaining.starts_with("fn") {
                self.advance_char_n(2);
                Ok(Token::Fn)
            } else if c == ';' {
                self.advance_char();
                Ok(Token::Semicolon)
            } else if c.is_ascii_digit() {
                let mut end = start;

                loop {
                    match self.current_char {
                        Some((index, c)) if c.is_ascii_digit() => {
                            end = index + c.len_utf8();
                            self.advance_char();
                        }
                        _ => break,
                    }
                }
                Ok(Token::Number(self.input[start..end].to_string()))
            } else if c.is_alphanumeric() {
                let mut end = start;

                loop {
                    match self.current_char {
                        Some((index, c)) if c.is_alphanumeric() => {
                            end = index + c.len_utf8();
                            self.advance_char();
                        }
                        _ => break,
                    }
                }
                Ok(Token::Identifier(self.input[start..end].to_string()))
            } else {
                Err(format!("Unknown token at {start}"))
            }
        } else {
            Ok(Token::Eof)
        }
    }

    fn queue_expected(&mut self, expected_one_of: Vec<Token>) {
        self.expected_queue.push_back(expected_one_of);
    }

    fn parse(&mut self) -> Result<(), String> {
        // parse tokens
        loop {
            self.skip_whitespace();

            let token = self.parse_token()?;

            self.tokens.push(token.clone());

            println!("Token: {:?}", token);

            if let Some(expected) = self.expected_queue.pop_front() {
                self.expect_one_of(expected)?;
            }

            if token == Token::Semicolon {
                self.expected_queue.clear();
            }

            if self.expected_queue.is_empty() {
                match token {
                    Token::Let => {
                        self.queue_expected([Token::Identifier(String::new())].into());
                        self.queue_expected([Token::Semicolon, Token::Equal].into());
                        self.queue_expected(
                            [
                                Token::Identifier(String::new()),
                                Token::Number(String::new()),
                                Token::StringLiteral(String::new()),
                            ]
                            .into(),
                        );
                    }
                    Token::Semicolon => {
                        self.queue_expected(
                            [
                                Token::Let,
                                Token::If,
                                Token::While,
                                Token::Eof,
                                Token::Identifier(String::new()),
                            ]
                            .into(),
                        );
                    }
                    Token::Identifier(_) | Token::Number(_) => {
                        self.queue_expected(
                            [
                                Token::Semicolon,
                                Token::Equal,
                                Token::Plus,
                                Token::Minus,
                                Token::Star,
                                Token::Slash,
                            ]
                            .into(),
                        );
                        self.queue_expected(
                            [
                                Token::Identifier(String::new()),
                                Token::Number(String::new()),
                                Token::StringLiteral(String::new()),
                            ]
                            .into(),
                        );
                    }

                    _ => {}
                }
                println!("{:?}", self.expected_queue);
            }

            if token == Token::Eof {
                break;
            }
        }

        Ok(())
    }
}

fn main() {
    let s = "let a = 0;".to_string();
    let mut p = Parser::new(&s);
    match p.parse() {
        Ok(()) => {}
        Err(str) => {
            println!("Error: {str}");
        }
    };
}
