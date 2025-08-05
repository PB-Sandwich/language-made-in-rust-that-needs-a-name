use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
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
    Identifier,
    Number,
    StringLiteral,

    // Special
    Eof,
    Any,
    Unknown, // For unexpected characters
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Token {
    kind: TokenKind,
    lexeme: String,
}

struct Lexer<'a> {
    input: &'a str,
    iter: std::str::CharIndices<'a>,
    current_char: Option<(usize, char)>, // (position, current character)
    tokens: Vec<Token>,
    expected_queue: VecDeque<Vec<TokenKind>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a String) -> Self {
        let mut iter = input.char_indices();
        let current_char = iter.next();
        let tokens = Vec::new();
        let expected_queue = VecDeque::new();
        Lexer {
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

    fn expect_one_of(&self, expected: Vec<TokenKind>) -> Result<(), String> {
        if let Some(last_token) = self.tokens.last() {
            if expected.iter().any(|t| t == &TokenKind::Any) {
                return Ok(());
            }

            let last_token_kind = last_token.kind.clone();

            if expected.iter().any(|t| t == &last_token_kind) {
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
                Ok(Token {
                    kind: TokenKind::Let,
                    lexeme: "let".into(),
                })
            } else if c == '=' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Equal,
                    lexeme: "=".into(),
                })
            } else if c == '+' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Plus,
                    lexeme: "+".into(),
                })
            } else if c == '-' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Minus,
                    lexeme: "-".into(),
                })
            } else if c == '*' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Star,
                    lexeme: "*".into(),
                })
            } else if c == '/' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Slash,
                    lexeme: "/".into(),
                })
            } else if c == '(' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::LParen,
                    lexeme: "(".into(),
                })
            } else if c == ')' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::RParen,
                    lexeme: ")".into(),
                })
            } else if c == '{' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::LBrace,
                    lexeme: "{".into(),
                })
            } else if c == '}' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::RBrace,
                    lexeme: "}".into(),
                })
            } else if c == ',' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Comma,
                    lexeme: ",".into(),
                })
            } else if remaining.starts_with("if") {
                self.advance_char_n(2);
                Ok(Token {
                    kind: TokenKind::If,
                    lexeme: "if".into(),
                })
            } else if remaining.starts_with("else") {
                self.advance_char_n(4);
                Ok(Token {
                    kind: TokenKind::Else,
                    lexeme: "else".into(),
                })
            } else if remaining.starts_with("while") {
                self.advance_char_n(5);
                Ok(Token {
                    kind: TokenKind::While,
                    lexeme: "while".into(),
                })
            } else if remaining.starts_with("fn") {
                self.advance_char_n(2);
                Ok(Token {
                    kind: TokenKind::Fn,
                    lexeme: "fn".into(),
                })
            } else if c == ';' {
                self.advance_char();
                Ok(Token {
                    kind: TokenKind::Semicolon,
                    lexeme: ";".into(),
                })
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
                Ok(Token {
                    kind: TokenKind::Number,
                    lexeme: self.input[start..end].to_string(),
                })
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
                Ok(Token {
                    kind: TokenKind::Identifier,
                    lexeme: self.input[start..end].to_string(),
                })
            } else {
                Err(format!("Unknown token at {start}"))
            }
        } else {
            Ok(Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
            })
        }
    }

    fn queue_expected(&mut self, expected_one_of: Vec<TokenKind>) {
        self.expected_queue.push_back(expected_one_of);
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        // parse tokens
        loop {
            self.skip_whitespace();

            let token = self.parse_token()?;

            self.tokens.push(token.clone());

            println!("Token: {:?}", token);

            if let Some(expected) = self.expected_queue.pop_front() {
                self.expect_one_of(expected)?;
            }

            if token.kind == TokenKind::Semicolon {
                self.expected_queue.clear();
            }

            if self.expected_queue.is_empty() {
                match token.kind {
                    TokenKind::Let => {
                        self.queue_expected([TokenKind::Identifier].into());
                        self.queue_expected([TokenKind::Semicolon, TokenKind::Equal].into());
                        self.queue_expected(
                            [
                                TokenKind::Identifier,
                                TokenKind::Number,
                                TokenKind::StringLiteral,
                            ]
                            .into(),
                        );
                    }
                    TokenKind::Semicolon => {
                        self.queue_expected(
                            [
                                TokenKind::Let,
                                TokenKind::If,
                                TokenKind::While,
                                TokenKind::Eof,
                                TokenKind::Identifier,
                            ]
                            .into(),
                        );
                    }
                    TokenKind::Identifier | TokenKind::Number => {
                        self.queue_expected(
                            [
                                TokenKind::Semicolon,
                                TokenKind::Equal,
                                TokenKind::Plus,
                                TokenKind::Minus,
                                TokenKind::Star,
                                TokenKind::Slash,
                            ]
                            .into(),
                        );
                        self.queue_expected(
                            [
                                TokenKind::Identifier,
                                TokenKind::Number,
                                TokenKind::StringLiteral,
                            ]
                            .into(),
                        );
                    }

                    _ => {}
                }
                println!("{:?}", self.expected_queue);
            }

            if token.kind == TokenKind::Eof {
                break;
            }
        }

        Ok(self.tokens.clone())
    }
}

fn main() {
    let s = "let a = 0; a = a + 1;".to_string();
    let mut p = Lexer::new(&s);
    match p.tokenize() {
        Ok(_) => {}
        Err(str) => {
            println!("Error: {str}");
        }
    };
}
