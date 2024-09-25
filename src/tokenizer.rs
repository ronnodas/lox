use std::iter::from_fn;
use std::num::ParseFloatError;
use std::ops::BitOr;
use std::str::Chars;

use miette::Diagnostic;
use thiserror::Error;

pub struct Tokenizer<'t> {
    reader: Reader<'t>,
    state: State,
}

#[derive(Debug, Clone, Copy)]
enum State {
    New,
    Slash,
    TestEqual(TestEqual),
    String,
    Number,
    Identifier,
    LineComment,
    BlockComment(usize),
    Errored,
}

#[derive(Debug, Clone, Copy)]
enum TestEqual {
    Bang,
    Equal,
    Less,
    Greater,
}

impl TestEqual {
    const fn equal<'t>(self) -> TokenKind<'t> {
        match self {
            Self::Bang => TokenKind::BangEqual,
            Self::Equal => TokenKind::EqualEqual,
            Self::Less => TokenKind::LessEqual,
            Self::Greater => TokenKind::GreaterEqual,
        }
    }

    const fn not_equal<'t>(self) -> TokenKind<'t> {
        match self {
            Self::Bang => TokenKind::Bang,
            Self::Equal => TokenKind::Equal,
            Self::Less => TokenKind::Less,
            Self::Greater => TokenKind::Greater,
        }
    }
}

//TODO add a Seen(char) state to reduce (eliminate?) peeking
impl<'t> Iterator for Tokenizer<'t> {
    type Item = Result<Token<'t>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = loop {
            match self.state {
                State::New => {
                    self.reader.save();
                    self.reader.trim_start();

                    let c = self.reader.next()?;
                    if let Some(token) = TokenKind::single_char(c) {
                        break token;
                    }

                    self.state = match c {
                        '!' => State::TestEqual(TestEqual::Bang),
                        '=' => State::TestEqual(TestEqual::Equal),
                        '<' => State::TestEqual(TestEqual::Less),
                        '>' => State::TestEqual(TestEqual::Greater),
                        '/' => State::Slash,
                        '"' => State::String,
                        '0'..='9' => State::Number,
                        '_' => State::Identifier,
                        c if c.is_alphabetic() => State::Identifier,

                        c => return Some(Err(self.error(ErrorKind::UnexpectedCharacter(c)))),
                    };
                }
                State::Slash => match self.reader.peek() {
                    Some('/') => {
                        _ = self.reader.next();
                        self.state = State::LineComment;
                    }
                    Some('*') => {
                        _ = self.reader.next();
                        self.state = State::BlockComment(0);
                    }
                    None | Some(_) => {
                        break TokenKind::Slash;
                    }
                },
                State::String => return Some(self.string()),
                State::TestEqual(test) => {
                    break if self.reader.peek() == Some('=') {
                        _ = self.reader.next();
                        test.equal()
                    } else {
                        test.not_equal()
                    };
                }
                State::Number => return Some(self.number()),
                State::Identifier => return Some(Ok(self.identifier())),
                State::LineComment => {
                    for c in self.reader.by_ref() {
                        if c == '\n' {
                            break;
                        }
                    }
                    self.state = State::New;
                }
                State::BlockComment(depth) => {
                    while let Some(c) = self.reader.next() {
                        if c == '*' && self.reader.peek() == Some('/') {
                            _ = self.reader.next();
                            self.state =
                                depth.checked_sub(1).map_or(State::New, State::BlockComment);
                            break;
                        } else if c == '/' && self.reader.peek() == Some('*') {
                            _ = self.reader.next();
                            self.state = State::BlockComment(depth + 1);
                            break;
                        }
                    }
                    return Some(Err(self.error(ErrorKind::UnterminatedBlockComment)));
                }
                State::Errored => return None,
            };
        };

        let (_, span) = self.reader.split_yielded();
        self.state = State::New;
        Some(Ok(Token::new(token, span)))
    }
}

impl<'t> Tokenizer<'t> {
    pub fn new(source: &'t str) -> Self {
        Self {
            reader: Reader::new(source),
            state: State::New,
        }
    }

    fn string(&mut self) -> Result<Token<'t>, Error> {
        let mut skip = false;
        for c in self.reader.by_ref() {
            if skip {
                match c {
                    '\\' | '"' => skip = false,
                    _ => return Err(self.reader.error(ErrorKind::UnexpectedCharacter(c))),
                }
            } else {
                match c {
                    '\\' => skip = true,
                    '"' => {
                        let (string, span) = self.reader.split_yielded();
                        let string = string
                            .strip_prefix('"')
                            .and_then(|string| string.strip_suffix('"'))
                            .unwrap_or_else(|| unreachable!("began and ended with `\"`"));

                        self.state = State::New;
                        return Ok(Token::new(TokenKind::String(string), span));
                    }
                    _ => (),
                }
            }
        }
        Err(self.reader.error(ErrorKind::UnterminatedString))
    }

    fn number(&mut self) -> Result<Token<'t>, Error> {
        let mut found_dot = false;
        let (string, span) = match from_fn(|| self.reader.with_index()).find_map(|(at, c)| {
            if c.is_ascii_digit() {
                None
            } else if c == '.' && !found_dot {
                found_dot = true;
                None
            } else {
                Some(at)
            }
        }) {
            Some(at) => {
                let (string, span) = self.reader.split_at(at);
                if string.ends_with('.') {
                    self.reader.split_at(at - '.'.len_utf8())
                } else {
                    (string, span)
                }
            }
            None => self.reader.split_yielded(),
        };
        match string.parse() {
            Err(e) => Err(self.reader.error(ErrorKind::FloatParse(e))),
            Ok(number) => {
                self.state = State::New;
                Ok(Token::new(TokenKind::Number(number), span))
            }
        }
    }

    fn identifier(&mut self) -> Token<'t> {
        let (identifier, span) = match from_fn(|| self.reader.with_index())
            .find(|(_, c)| !c.is_alphanumeric() && c != &'_')
        {
            Some((at, _)) => self.reader.split_at(at),
            None => self.reader.split_yielded(),
        };
        let kind = TokenKind::keyword(identifier).unwrap_or(TokenKind::Identifier(identifier));

        self.state = State::New;
        Token::new(kind, span)
    }

    fn error(&mut self, error: ErrorKind) -> Error {
        self.state = State::Errored;
        self.reader.error(error)
    }
}

#[derive(Debug)]
struct Reader<'t> {
    source: &'t str,
    rest: &'t str,
    chars: Chars<'t>,
    yielded_bytes: usize,
}

impl<'t> Reader<'t> {
    fn new(source: &'t str) -> Self {
        Self {
            source,
            rest: source,
            chars: source.chars(),
            yielded_bytes: 0,
        }
    }

    fn save(&mut self) {
        self.rest = self.chars.as_str();
        self.yielded_bytes = 0;
    }

    fn error(&self, error: ErrorKind) -> Error {
        Error {
            source_code: self.source.to_owned(),
            bad_bit: self.span(),
            kind: error,
        }
    }

    fn split_yielded(&self) -> (&'t str, SourceSpan) {
        let (split, _) = self.rest.split_at(self.yielded_bytes);
        (split, self.span())
    }

    fn split_at(&mut self, at: usize) -> (&'t str, SourceSpan) {
        let (split, rest) = self.rest.split_at(at);
        self.chars = rest.chars();
        self.yielded_bytes = at;
        (split, self.span())
    }

    fn span(&self) -> SourceSpan {
        (self.source.len() - self.rest.len(), self.yielded_bytes).into()
    }

    fn trim_start(&mut self) {
        self.rest = self.rest.trim_start();
        self.chars = self.rest.chars();
        self.yielded_bytes = 0;
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn with_index(&mut self) -> Option<(usize, char)> {
        Some((self.yielded_bytes, self.next()?))
    }
}

impl Iterator for Reader<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        self.yielded_bytes += c.len_utf8();
        Some(c)
    }
}

#[derive(Error, Debug, Diagnostic, Clone, PartialEq, Eq)]
#[error("{kind}")]
pub struct Error {
    #[source_code]
    source_code: String,
    #[label]
    bad_bit: SourceSpan,
    kind: ErrorKind,
}

#[derive(Error, Debug, Diagnostic, Clone, PartialEq, Eq)]
enum ErrorKind {
    #[error("Unexpected character '{0}'")]
    UnexpectedCharacter(char),
    #[error("Unterminated string")]
    UnterminatedString,
    #[error("Failed to parse float '{0}'")]
    FloatParse(#[from] ParseFloatError),
    #[error("Unterminated block comment")]
    UnterminatedBlockComment,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'t> {
    pub kind: TokenKind<'t>,
    pub span: SourceSpan,
}

impl<'t> Token<'t> {
    const fn new(kind: TokenKind<'t>, span: SourceSpan) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'t> {
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
    Identifier(&'t str),
    String(&'t str),
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
}

impl<'t> TokenKind<'t> {
    const fn single_char(c: char) -> Option<Self> {
        let token = match c {
            '(' => Self::LeftParen,
            ')' => Self::RightParen,
            '{' => Self::LeftBrace,
            '}' => Self::RightBrace,
            ',' => Self::Comma,
            '.' => Self::Dot,
            '-' => Self::Minus,
            '+' => Self::Plus,
            ';' => Self::Semicolon,
            '*' => Self::Star,
            _ => return None,
        };
        Some(token)
    }

    fn keyword(identifier: &str) -> Option<Self> {
        let keyword = match identifier {
            "and" => Self::And,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::False,
            "for" => Self::For,
            "fun" => Self::Fun,
            "if" => Self::If,
            "nil" => Self::Nil,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    start: usize,
    len: usize,
}

impl SourceSpan {
    const fn end(&self) -> usize {
        self.start + self.len
    }
}

impl From<(usize, usize)> for SourceSpan {
    fn from((start, len): (usize, usize)) -> Self {
        Self { start, len }
    }
}

impl From<SourceSpan> for miette::SourceSpan {
    fn from(value: SourceSpan) -> Self {
        (value.start, value.len).into()
    }
}

#[expect(
    clippy::suspicious_arithmetic_impl,
    reason = "easier to find end than len"
)]
impl BitOr for SourceSpan {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let start = self.start.min(rhs.start);
        Self {
            start,
            len: self.end().max(rhs.end()) - start,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::{TokenKind, Tokenizer};

    #[test]
    fn tokenizer() -> miette::Result<()> {
        let source = "2 < 3";

        let mut tokenizer = Tokenizer::new(source);
        for expected in [
            TokenKind::Number(2.0),
            TokenKind::Less,
            TokenKind::Number(3.0),
        ] {
            let kind = tokenizer.next().unwrap()?.kind;
            assert_eq!(kind, expected);
        }
        assert!(tokenizer.next().is_none());
        Ok(())
    }
}
