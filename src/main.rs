#[derive(Debug, PartialEq, Eq, Clone)]

pub enum TokenKind {
    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    LParen,
    RParen,

    Number,

    Eof,
    Unknown, // For unexpected characters
}

#[derive(Debug, PartialEq, Clone)]
struct Token {
    kind: TokenKind,
    lexeme: String,
    value: f64,
}

type TK = TokenKind;

struct Lexer<'a> {
    input: &'a str,
    iter: std::str::CharIndices<'a>,
    current_char: Option<(usize, char)>, // (position, current character)
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a String) -> Self {
        let mut iter = input.char_indices();
        let current_char = iter.next();
        let tokens = Vec::new();
        Lexer {
            input,
            iter,
            current_char,
            tokens,
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

            if c == '=' {
                self.advance_char();
                Ok(Token {
                    kind: TK::Equal,
                    lexeme: "=".into(),
                    value: 0.0,
                })
            } else if c == '+' {
                self.advance_char();
                Ok(Token {
                    kind: TK::Plus,
                    lexeme: "+".into(),
                    value: 0.0,
                })
            } else if c == '-' {
                self.advance_char();
                Ok(Token {
                    kind: TK::Minus,
                    lexeme: "-".into(),
                    value: 0.0,
                })
            } else if c == '*' {
                self.advance_char();
                Ok(Token {
                    kind: TK::Star,
                    lexeme: "*".into(),
                    value: 0.0,
                })
            } else if c == '/' {
                self.advance_char();
                Ok(Token {
                    kind: TK::Slash,
                    lexeme: "/".into(),
                    value: 0.0,
                })
            } else if c == '(' {
                self.advance_char();
                Ok(Token {
                    kind: TK::LParen,
                    lexeme: "(".into(),
                    value: 0.0,
                })
            } else if c == ')' {
                self.advance_char();
                Ok(Token {
                    kind: TK::RParen,
                    lexeme: ")".into(),
                    value: 0.0,
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
                    kind: TK::Number,
                    lexeme: self.input[start..end].to_string(),
                    value: self.input[start..end].to_string().parse().unwrap(),
                })
            } else {
                Err(format!("Unknown token at {start}"))
            }
        } else {
            Ok(Token {
                kind: TK::Eof,
                lexeme: String::new(),
                value: 0.0,
            })
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        // parse tokens
        loop {
            self.skip_whitespace();

            let token = self.parse_token()?;

            self.tokens.push(token.clone());

            if token.kind == TK::Eof {
                break;
            }
        }

        Ok(self.tokens.clone())
    }
}

enum ParseNode {
    Number(f64),
    BinaryOp {
        left: Box<ParseNode>,
        op: Token,
        right: Box<ParseNode>,
    },
}

impl ParseNode {
    pub fn print(&self, indent: usize) {
        let pad = "  ".repeat(indent);
        match self {
            ParseNode::Number(n) => {
                println!("{}Number({})", pad, n);
            }
            ParseNode::BinaryOp { left, op, right } => {
                println!("{}BinaryOp({:?})", pad, op.kind);
                left.print(indent + 1);
                right.print(indent + 1);
            }
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    cur_token: Token,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.clone(),
            cur_token: tokens[0].clone(),
            pos: 0,
        }
    }

    fn expect_token(&mut self, expected: TokenKind) {
        if self.cur_token.kind == expected {
            self.advance_token();
        } else {
            panic!(
                "Expected {:?}, but found {:?}",
                expected, self.cur_token.kind
            );
        }
    }

    fn advance_token(&mut self) {
        self.pos += 1;
        self.cur_token = self.tokens[self.pos].clone();
    }

    fn parse_expression(&mut self) -> ParseNode {
        let mut node = self.parse_term();
        while self.cur_token.kind == TokenKind::Plus || self.cur_token.kind == TokenKind::Minus {
            let op = self.cur_token.clone();
            self.advance_token();
            let right = self.parse_term();
            node = ParseNode::BinaryOp {
                left: Box::new(node),
                op,
                right: Box::new(right),
            };
        }
        node
    }

    fn parse_term(&mut self) -> ParseNode {
        let mut node = self.parse_factor();
        while self.cur_token.kind == TokenKind::Star || self.cur_token.kind == TokenKind::Slash {
            let op = self.cur_token.clone();
            self.advance_token();
            let right = self.parse_factor();
            node = ParseNode::BinaryOp {
                left: Box::new(node),
                op,
                right: Box::new(right),
            };
        }
        node
    }

    fn parse_factor(&mut self) -> ParseNode {
        match self.cur_token.kind {
            TokenKind::Number => {
                let n = self.cur_token.value;
                self.advance_token();
                return ParseNode::Number(n);
            }
            TokenKind::LParen => {
                self.advance_token();
                let expr = self.parse_expression();
                self.expect_token(TokenKind::RParen);
                expr
            }
            _ => panic!("Unexpected token"),
        }
    }
}

fn evaluate(expr: &ParseNode) -> f64 {
    match expr {
        ParseNode::Number(n) => *n,
        ParseNode::BinaryOp { left, op, right } => {
            let l = evaluate(left);
            let r = evaluate(right);
            match op.kind {
                TokenKind::Plus => l + r,
                TokenKind::Minus => l - r,
                TokenKind::Star => l * r,
                TokenKind::Slash => l / r,
                _ => panic!("Unknown op"),
            }
        }
    }
}

fn main() {
    let input = "(1 + 4) * 8".to_string();
    let mut lexer = Lexer::new(&input);
    let tokens = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens);
    println!("{}", evaluate(&parser.parse_expression()));
}
