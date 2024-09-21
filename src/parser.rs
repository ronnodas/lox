pub mod ast;

use std::{error, fmt};

use ast::{Equality, Expression, MatchToken, Parse, ParseTower, Primary, Unary, Value};

use crate::tokenizer::{SourceToken, Token};

pub struct Parser<'p, 't> {
    tokens: &'p [SourceToken<'t>],
}

impl<'p, 't> Parser<'p, 't> {
    pub const fn new(tokens: &'p [SourceToken<'t>]) -> Self {
        Self { tokens }
    }

    pub fn expression(&mut self) -> Result<Option<Expression<'t>>, Error<'t>> {
        self.parse::<Equality>().map(|e| e.map(Expression::from))
    }

    fn parse<T: Parse<'t>>(&mut self) -> Result<Option<T>, Error<'t>> {
        T::parse(self)
    }

    fn parse_tower<T: ParseTower<'t>>(&mut self) -> Result<Option<T>, Error<'t>> {
        let Some(start) = self.parse()? else {
            return Ok(None);
        };
        let mut more = Vec::new();
        while let Some(operator) = self.try_read_any() {
            let Some(right) = self.parse()? else {
                return Err(self.unmatched());
            };
            more.push((operator, right));
        }
        Ok(Some(T::new(start, more)))
    }

    const fn unmatched(&self) -> Error<'t> {
        match self.tokens.first() {
            Some(&token) => Error::UnexpectedToken(token),
            None => Error::UnexpectedEndOfInput,
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

    fn literal(&mut self) -> Result<Option<Primary<'t>>, Error<'t>> {
        if let Some(primary) = self.try_read_any::<Value<'t>>() {
            return Ok(Some(primary.into()));
        }
        let Some((
            &SourceToken {
                line,
                token: Token::LeftParen,
            },
            rest,
        )) = self.tokens.split_first()
        else {
            return Ok(None);
        };
        self.tokens = rest;
        let Some(expression) = self.parse()? else {
            let error = match self.tokens.first() {
                Some(&token) => Error::UnexpectedToken(token),
                None => Error::UnmatchedParen(line),
            };
            return Err(error);
        };
        match self.tokens.split_first() {
            None => Err(Error::UnmatchedParen(line)),
            Some((
                SourceToken {
                    token: Token::RightParen,
                    ..
                },
                rest,
            )) => {
                self.tokens = rest;
                Ok(Some(Primary::group(expression)))
            }
            Some((&token, _)) => Err(Error::UnexpectedToken(token)),
        }
    }

    fn unary(&mut self) -> Result<Option<Unary<'t>>, Error<'t>> {
        if let Some(operator) = self.try_read_any() {
            let Some(right) = self.parse()? else {
                return Err(self.unmatched());
            };
            Ok(Some(Unary::Unary(operator, Box::new(right))))
        } else {
            self.parse::<Primary<'t>>().map(|e| e.map(Unary::from))
        }
    }
}

#[derive(Debug)]
pub enum Error<'t> {
    UnexpectedToken(SourceToken<'t>),
    UnmatchedParen(usize),
    UnexpectedEndOfInput,
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedToken(source_token) => {
                write!(f, "Unexpected token while parsing: {source_token:?}")
            }
            Error::UnmatchedParen(line) => write!(f, "Unmatched parenthesis on line {line}"),
            Error::UnexpectedEndOfInput => write!(f, "Input ended before parsing finished"),
        }
    }
}

impl error::Error for Error<'_> {}
