pub mod ast;

use std::{error, fmt};

use ast::{Equality, Expression, MatchToken, Parse, ParseTower, Primary, Statement, Unary, Value};

use crate::tokenizer::{SourceToken, Token};

pub struct Parser<'p, 't> {
    tokens: &'p [SourceToken<'t>],
}

impl<'p, 't> Parser<'p, 't> {
    pub const fn new(tokens: &'p [SourceToken<'t>]) -> Self {
        Self { tokens }
    }

    pub fn into_parsed(mut self) -> Result<Vec<Statement<'t>>, Error<'t>> {
        let mut statements = Vec::new();
        while let Some(statement) = self.statement()? {
            statements.push(statement);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Option<Statement<'t>>, Error<'t>> {
        self.print_statement()?.map_or_else(
            || self.expression_statement(),
            |statement| Ok(Some(statement)),
        )
    }

    fn print_statement(&mut self) -> Result<Option<Statement<'t>>, Error<'t>> {
        if let Ok(&SourceToken { line, .. }) = self.consume(&Token::Print) {
            let Some(expression) = self.expression()? else {
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
        let Some(expression) = self.expression()? else {
            return Ok(None);
        };

        if self.consume(&Token::Semicolon).is_ok() {
            Ok(Some(Statement::Expression(expression)))
        } else {
            Err(self.unexpected())
        }
    }

    pub fn expression(&mut self) -> Result<Option<Expression<'t>>, Error<'t>> {
        self.parse::<Equality>().map(|e| e.map(Expression::from))
    }

    fn parse<T: Parse<'t>>(&mut self) -> Result<Option<T>, Error<'t>> {
        T::parse(self)
    }

    fn parse_fold<T: ParseTower<'t>>(&mut self) -> Result<Option<T>, Error<'t>> {
        let Some(start) = self.parse()? else {
            return Ok(None);
        };
        let mut more = Vec::new();
        while let Some(operator) = self.try_read_any() {
            let Some(right) = self.parse()? else {
                return Err(self.unexpected());
            };
            more.push((operator, right));
        }
        Ok(Some(T::new(start, more)))
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
                return Err(self.unexpected());
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
