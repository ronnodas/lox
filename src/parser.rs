pub mod ast;

use std::iter::from_fn;
use std::{error, fmt};

use ast::{Equality, Expression, Fold, MatchToken, Primary, Statement, Unary, Value};

use crate::tokenizer::{SourceToken, Token};

pub struct Parser<'p, 't> {
    tokens: &'p [SourceToken<'t>],
}

impl<'t, 'p> Parser<'p, 't> {
    pub const fn new(tokens: &'p [SourceToken<'t>]) -> Self {
        Self { tokens }
    }

    pub fn into_statements(
        mut self,
    ) -> impl Iterator<Item = Result<Statement<'t>, Error<'t>>> + 'p {
        from_fn(move || {
            let statement = self.parse().transpose()?;
            if statement.is_err() {
                self.recover();
            }
            Some(statement)
        })
    }

    fn parse<T: Parse<'t>>(&mut self) -> Result<Option<T>, Error<'t>> {
        T::parse(self)
    }

    fn print_statement(&mut self) -> Result<Option<Statement<'t>>, Error<'t>> {
        if let Ok(&SourceToken { line, .. }) = self.consume(&Token::Print) {
            let Some(expression) = self.parse()? else {
                return Err(self.unexpected());
            };

            if self.consume(&Token::Semicolon).is_ok() {
                Ok(Some(Statement::Print(expression)))
            } else {
                Err(Error::UnterminatedPrint(line))
            }
        } else {
            Ok(None)
        }
    }

    fn expression_statement(&mut self) -> Result<Option<Statement<'t>>, Error<'t>> {
        let Some(expression) = self.parse()? else {
            return Ok(None);
        };

        if self.consume(&Token::Semicolon).is_ok() {
            Ok(Some(Statement::Expression(expression)))
        } else {
            Err(self.unexpected())
        }
    }

    const fn unexpected(&self) -> Error<'t> {
        match self.tokens.first() {
            Some(&token) => Error::UnexpectedToken(token),
            None => Error::UnexpectedEndOfInput,
        }
    }

    fn consume(&mut self, token: &Token) -> Result<&SourceToken, Option<&SourceToken>> {
        if let Some((next @ SourceToken { token: t, .. }, rest)) = self.tokens.split_first() {
            if t == token {
                self.tokens = rest;
                Ok(next)
            } else {
                Err(Some(next))
            }
        } else {
            Err(None)
        }
    }

    fn try_read_any<T: MatchToken<'t>>(&mut self) -> Option<T> {
        let (next, rest) = self.tokens.split_first()?;
        if let Some(next) = T::match_token(next.token) {
            self.tokens = rest;
            Some(next)
        } else {
            None
        }
    }

    fn recover(&mut self) {
        let mid = if let Some(mid) = self.tokens.iter().position(|t| t.token == Token::Semicolon) {
            mid
        } else {
            self.tokens.len()
        };
        self.tokens = &self.tokens[mid..];
    }
}

pub trait Parse<'t>: Sized {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>>;
}

//TODO maybe inline and restructure once all the different statements are implemented
impl<'t> Parse<'t> for Statement<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let statement @ Some(_) = parser.print_statement()? {
            return Ok(statement);
        }
        parser.expression_statement()
    }
}

impl<'t> Parse<'t> for Expression<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        parser.parse::<Equality>().map(|e| e.map(Self::from))
    }
}

impl<'t, T: Parse<'t>, Op: MatchToken<'t>> Parse<'t> for Fold<T, Op> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        let this = &mut *parser;
        let Some(start) = this.parse()? else {
            return Ok(None);
        };
        let mut more = Vec::new();
        while let Some(operator) = this.try_read_any() {
            let Some(right) = this.parse()? else {
                return Err(this.unexpected());
            };
            more.push((operator, right));
        }
        Ok(Some(Self { start, more }))
    }
}

impl<'t> Parse<'t> for Unary<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let Some(operator) = parser.try_read_any() {
            let Some(right) = parser.parse()? else {
                return Err(parser.unexpected());
            };
            Ok(Some(Self::Unary(operator, Box::new(right))))
        } else {
            parser.parse::<Primary<'t>>().map(|e| e.map(Self::from))
        }
    }
}

impl<'t> Parse<'t> for Primary<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let Some(primary) = parser.try_read_any::<Value<'t>>() {
            return Ok(Some(primary.into()));
        }
        let Some((
            &SourceToken {
                line,
                token: Token::LeftParen,
            },
            rest,
        )) = parser.tokens.split_first()
        else {
            return Ok(None);
        };
        parser.tokens = rest;
        let Some(expression) = parser.parse()? else {
            let error = match parser.tokens.first() {
                Some(&token) => Error::UnexpectedToken(token),
                None => Error::UnmatchedParen(line),
            };
            return Err(error);
        };
        match parser.tokens.split_first() {
            None => Err(Error::UnmatchedParen(line)),
            Some((
                SourceToken {
                    token: Token::RightParen,
                    ..
                },
                rest,
            )) => {
                parser.tokens = rest;
                Ok(Some(Primary::group(expression)))
            }
            Some((&token, _)) => Err(Error::UnexpectedToken(token)),
        }
    }
}

#[derive(Debug)]
pub enum Error<'t> {
    UnexpectedToken(SourceToken<'t>),
    UnmatchedParen(usize),
    UnexpectedEndOfInput,
    UnterminatedPrint(usize),
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedToken(source_token) => {
                write!(f, "Unexpected token while parsing: {source_token:?}")
            }
            Error::UnmatchedParen(line) => write!(f, "Unmatched parenthesis on line {line}"),
            Error::UnexpectedEndOfInput => write!(f, "Input ended before parsing finished"),
            Error::UnterminatedPrint(line) => {
                write!(f, "Unterminated print statement on line {line}")
            }
        }
    }
}

impl error::Error for Error<'_> {}
