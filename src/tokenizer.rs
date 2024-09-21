use std::num::ParseFloatError;
use std::{error, fmt};

pub struct Tokenizer<'a> {
    source: &'a str,
    line: usize,
}

impl<'a> Tokenizer<'a> {
    pub const fn new(source: &'a str) -> Self {
        Self { source, line: 1 }
    }

    pub fn into_tokens(mut self) -> Result<Vec<SourceToken<'a>>, Error> {
        let mut tokens = Vec::new();
        while let Some(token) = self.scan_token() {
            tokens.push(token?);
        }

        tokens.push(SourceToken::new(Token::Eof, self.line));
        Ok(tokens)
    }

    // TODO Add support for C-style /* ... */ block comments.
    // Make sure to handle newlines in them. Consider allowing them to nest.
    fn scan_token(&mut self) -> Option<Result<SourceToken<'a>, Error>> {
        let token_type = loop {
            let (c, rest) = split_first_char(self.source)?;
            let backtrack = self.source;
            self.source = rest;
            break match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                ',' => Token::Comma,
                '.' => Token::Dot,
                '-' => Token::Minus,
                '+' => Token::Plus,
                ';' => Token::Semicolon,
                '*' => Token::Star,

                '!' => self.if_next_equal(Token::BangEqual, Token::Bang),
                '=' => self.if_next_equal(Token::EqualEqual, Token::Equal),
                '<' => self.if_next_equal(Token::LessEqual, Token::Less),
                '>' => self.if_next_equal(Token::GreaterEqual, Token::Greater),

                '/' => {
                    if self.try_read('/') {
                        match self.split_once('\n') {
                            Some(_comment) => {
                                self.line += 1;
                            }
                            None => {
                                // comment terminated by eof
                                self.source = "";
                            }
                        }
                        continue;
                    }
                    Token::Slash
                }

                ' ' | '\r' | '\t' => continue,

                '\n' => {
                    self.line += 1;
                    continue;
                }

                //TODO allow \" (and therefore \\) in strings
                '"' => match self.split_once('"') {
                    Some(string) => Token::String(string),
                    None => return Some(Err(Error::UnterminatedString(self.line))),
                },

                '0'..='9' => {
                    self.source = backtrack;
                    match self.number() {
                        Ok(number) => Token::Number(number),
                        Err(error) => return Some(Err(error)),
                    }
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    self.source = backtrack;
                    self.identifier()
                }

                c => return Some(Err(Error::UnexpectedCharacter(c, self.line))),
            };
        };
        Some(Ok(SourceToken::new(token_type, self.line)))
    }

    fn if_next_equal(&mut self, yes: Token<'a>, no: Token<'a>) -> Token<'a> {
        if self.try_read('=') {
            yes
        } else {
            no
        }
    }

    fn try_read(&mut self, c: char) -> bool {
        let Some((next, rest)) = split_first_char(self.source) else {
            return false;
        };
        if next == c {
            self.source = rest;
            true
        } else {
            false
        }
    }

    fn split_once(&mut self, delimiter: char) -> Option<&'a str> {
        match self.source.split_once(delimiter) {
            Some((prefix, rest)) => {
                self.source = rest;
                self.line += prefix.matches('\n').count();
                Some(prefix)
            }
            None => None,
        }
    }

    fn number(&mut self) -> Result<f64, Error> {
        let mut found_dot = false;
        let mid = self.source.find(|c: char| {
            if c.is_ascii_digit() {
                false
            } else if !found_dot && c == '.' {
                found_dot = true;
                false
            } else {
                true
            }
        });

        let (number, rest) = match mid {
            Some(mid) => {
                let (number, rest) = self.source.split_at(mid);
                if number.ends_with('.') {
                    self.source.split_at(mid - 1)
                } else {
                    (number, rest)
                }
            }
            None => (self.source, ""),
        };
        self.source = rest;
        number.parse().map_err(|e| Error::FloatParse(e, self.line))
    }

    fn identifier(&mut self) -> Token<'a> {
        let (identifier, rest) = self
            .source
            .find(|c: char| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
            .map_or((self.source, ""), |mid| self.source.split_at(mid));
        self.source = rest;

        Token::keyword(identifier).unwrap_or(Token::Identifier(identifier))
    }
}

fn split_first_char(s: &str) -> Option<(char, &str)> {
    let mut chars = s.chars();
    chars.next().map(|c| (c, chars.as_str()))
}

#[derive(Debug)]
pub enum Error {
    UnexpectedCharacter(char, usize),
    UnterminatedString(usize),
    FloatParse(ParseFloatError, usize),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter(c, line) => {
                write!(f, "Unexpected character: {c:?} on line {line}")
            }
            Self::UnterminatedString(line) => {
                write!(f, "Unterminated string starting on line {line}")
            }
            Self::FloatParse(error, line) => {
                write!(f, "Failed to parse float on line {line}: {error}")
            }
        }
    }
}

impl error::Error for Error {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(&'a str),
    String(&'a str),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl<'a> Token<'a> {
    pub fn keyword(identifier: &str) -> Option<Self> {
        let keyword = match identifier {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "for" => Token::For,
            "fun" => Token::Fun,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Self::Or,
            "print" => Self::Print,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::True,
            "var" => Self::Var,
            "while" => Self::While,

            _ => return None,
        };
        Some(keyword)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceToken<'a> {
    pub token: Token<'a>,
    pub line: usize,
}

impl<'a> SourceToken<'a> {
    pub const fn new(token: Token<'a>, line: usize) -> Self {
        Self { token, line }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer() {
        let source = "2 < 3";

        let tokenizer = Tokenizer::new(source);
        assert_eq!(
            &tokenizer.into_tokens().unwrap(),
            &[
                SourceToken {
                    token: Token::Number(2.0),
                    line: 1
                },
                SourceToken {
                    token: Token::Less,
                    line: 1
                },
                SourceToken {
                    token: Token::Number(3.0),
                    line: 1
                },
                SourceToken {
                    token: Token::Eof,
                    line: 1
                }
            ]
        );
    }
}
