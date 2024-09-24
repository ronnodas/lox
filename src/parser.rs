pub mod ast;
pub mod printer;

use std::iter::from_fn;
use std::{error, fmt};

use ast::{
    AndOperator, Assignment, ComparisonOperator, Declaration, EqualityOperator, Expression,
    FactorOperator, Fold, Identifier, IntoLValue as _, LogicalOr, OrOperator, Primary, Statement,
    SumOperator, Unary, UnaryOperator, Value,
};
use itertools::Itertools as _;

use crate::tokenizer::{SourceToken, Token};

pub struct Parser<'p, 't> {
    tokens: &'p [SourceToken<'t>],
}

impl<'t, 'p> Parser<'p, 't> {
    pub const fn new(tokens: &'p [SourceToken<'t>]) -> Self {
        Self { tokens }
    }

    pub fn into_statements(mut self) -> impl Iterator<Item = Result<Declaration, Error<'t>>> + 'p {
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

    fn print_statement(&mut self) -> Result<Option<Statement>, Error<'t>> {
        let Ok(SourceToken { line, .. }) = self.consume(&Token::Print) else {
            return Ok(None);
        };
        let Some(expression) = self.parse()? else {
            return Err(self.unexpected());
        };

        if self.consume(&Token::Semicolon).is_ok() {
            Ok(Some(Statement::Print(expression)))
        } else {
            Err(Error::UnterminatedPrint(line))
        }
    }

    fn expression_statement(&mut self) -> Result<Option<Statement>, Error<'t>> {
        let Some(expression) = self.parse()? else {
            return Ok(None);
        };
        _ = self.consume(&Token::Semicolon)?;
        Ok(Some(Statement::Expression(expression)))
    }

    fn block(&mut self) -> Result<Option<Statement>, Error<'t>> {
        if self.consume(&Token::LeftBrace).is_ok() {
            let declarations = from_fn(|| self.parse().transpose()).try_collect()?;
            _ = self.consume(&Token::RightBrace)?;
            Ok(Some(Statement::Block(declarations)))
        } else {
            Ok(None)
        }
    }

    const fn unexpected(&self) -> Error<'t> {
        match self.tokens.first() {
            Some(&token) => Error::UnexpectedToken(token),
            None => Error::UnexpectedEndOfInput,
        }
    }

    fn consume(&mut self, token: &Token) -> Result<SourceToken, Error<'t>> {
        if let Some((&next @ SourceToken { token: t, .. }, rest)) = self.tokens.split_first() {
            if &t == token {
                self.tokens = rest;
                Ok(next)
            } else {
                Err(Error::UnexpectedToken(next))
            }
        } else {
            Err(Error::UnexpectedEndOfInput)
        }
    }

    fn consume_identifier(&mut self) -> Option<Identifier> {
        if let (
            &SourceToken {
                token: Token::Identifier(identifier),
                ..
            },
            rest,
        ) = self.tokens.split_first()?
        {
            self.tokens = rest;
            Some(identifier.into())
        } else {
            None
        }
    }

    fn consume_and_match<T: MatchToken<'t>>(&mut self) -> Option<T> {
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

    fn variable_declaration(&mut self) -> Result<Option<Declaration>, Error<'t>> {
        if self.consume(&Token::Var).is_err() {
            return Ok(None);
        };
        let Some(Primary::Identifier(identifier)) = self.parse()? else {
            return Err(self.unexpected());
        };
        let initializer = if self.consume(&Token::Equal).is_ok() {
            self.parse()?
        } else {
            None
        };
        _ = self.consume(&Token::Semicolon)?;
        Ok(Some(Declaration::VariableDeclaration {
            identifier,
            initializer,
        }))
    }

    fn if_statement(&mut self) -> Result<Option<Statement>, Error<'t>> {
        if self.consume(&Token::If).is_err() {
            return Ok(None);
        }
        _ = self.consume(&Token::LeftParen)?;
        let Some(condition) = self.parse()? else {
            return Err(self.unexpected());
        };
        _ = self.consume(&Token::RightParen)?;
        let Some(then_branch) = self.parse()? else {
            return Err(self.unexpected());
        };
        let else_branch = if self.consume(&Token::Else).is_ok() {
            if let Some(else_branch) = self.parse()? {
                Some(Box::new(else_branch))
            } else {
                return Err(self.unexpected());
            }
        } else {
            None
        };

        Ok(Some(Statement::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        }))
    }

    fn while_statement(&mut self) -> Result<Option<Statement>, Error<'t>> {
        if self.consume(&Token::While).is_err() {
            return Ok(None);
        }
        _ = self.consume(&Token::LeftParen)?;
        let Some(condition) = self.parse()? else {
            return Err(self.unexpected());
        };
        _ = self.consume(&Token::RightParen)?;
        let Some(body) = self.parse()? else {
            return Err(self.unexpected());
        };
        Ok(Some(Statement::While {
            condition,
            body: Box::new(body),
        }))
    }

    fn for_statement(&mut self) -> Result<Option<Statement>, Error<'t>> {
        if self.consume(&Token::For).is_err() {
            return Ok(None);
        }
        _ = self.consume(&Token::LeftParen)?;
        let initializer = self.parse()?;
        if initializer.is_none() {
            _ = self.consume(&Token::Semicolon)?;
        }
        let condition = self.parse()?;
        _ = self.consume(&Token::Semicolon)?;
        let increment = self.parse()?;
        _ = self.consume(&Token::RightParen)?;
        let Some(body) = self.parse()? else {
            return Err(self.unexpected());
        };
        Ok(Some(desugar_for(initializer, condition, increment, body)))
    }
}

fn desugar_for(
    initializer: Option<ForInitializer>,
    condition: Option<Expression>,
    increment: Option<Expression>,
    body: Statement,
) -> Statement {
    let body = Box::new(match increment {
        Some(increment) => {
            Statement::Block(vec![body.into(), Statement::Expression(increment).into()])
        }
        None => body,
    });
    let condition = condition.unwrap_or_else(|| Value::Boolean(true).into());
    let body = Statement::While { condition, body };
    match initializer {
        Some(initializer) => Statement::Block(vec![initializer.into(), body.into()]),
        None => body,
    }
}

enum ForInitializer {
    Declaration(Declaration),
    Expression(Expression),
}

impl From<ForInitializer> for Declaration {
    fn from(initializer: ForInitializer) -> Self {
        match initializer {
            ForInitializer::Declaration(declaration) => declaration,
            ForInitializer::Expression(expression) => Statement::Expression(expression).into(),
        }
    }
}

pub trait Parse<'t>: Sized {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>>;
}

//TODO maybe inline and restructure once all the different statements are implemented
impl<'t> Parse<'t> for Declaration {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let declaration @ Some(_) = parser.variable_declaration()? {
            Ok(declaration)
        } else {
            Ok(parser.parse()?.map(Self::Statement))
        }
    }
}

impl<'t> Parse<'t> for Statement {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let statement @ Some(_) = parser.print_statement()? {
            Ok(statement)
        } else if let statement @ Some(_) = parser.block()? {
            Ok(statement)
        } else if let statement @ Some(_) = parser.if_statement()? {
            Ok(statement)
        } else if let statement @ Some(_) = parser.while_statement()? {
            Ok(statement)
        } else if let statement @ Some(_) = parser.for_statement()? {
            Ok(statement)
        } else {
            parser.expression_statement()
        }
    }
}

impl<'t> Parse<'t> for Expression {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        let Some(expression) = parser.parse::<LogicalOr>()? else {
            return Ok(None);
        };
        if parser.consume(&Token::Equal).is_err() {
            return Ok(Some(Self::Or(expression)));
        };
        let Some(lvalue) = expression.lvalue() else {
            return Err(Error::InvalidLValue(Self::Or(expression).to_string()));
        };
        let Some(expression) = parser.parse()? else {
            return Err(parser.unexpected());
        };
        Ok(Some(Self::Assignment(Assignment {
            lvalue,
            expression: Box::new(expression),
        })))
    }
}

impl<'t, T: Parse<'t>, Op: MatchToken<'t>> Parse<'t> for Fold<T, Op> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        let this = &mut *parser;
        let Some(start) = this.parse()? else {
            return Ok(None);
        };
        let mut more = Vec::new();
        while let Some(operator) = this.consume_and_match() {
            let Some(right) = this.parse()? else {
                return Err(this.unexpected());
            };
            more.push((operator, right));
        }
        Ok(Some(Self { start, more }))
    }
}

impl<'t> Parse<'t> for Unary {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let Some(operator) = parser.consume_and_match() {
            let Some(right) = parser.parse()? else {
                return Err(parser.unexpected());
            };
            Ok(Some(Self::Unary(operator, Box::new(right))))
        } else {
            parser.parse::<Primary>().map(|e| e.map(Self::from))
        }
    }
}

impl<'t> Parse<'t> for Primary {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let Some(primary) = parser.consume_and_match::<Value>() {
            return Ok(Some(primary.into()));
        }
        if let Some(identifier) = parser.consume_identifier() {
            return Ok(Some(Self::Identifier(identifier)));
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
                Ok(Some(Self::group(expression)))
            }
            Some((&token, _)) => Err(Error::UnexpectedToken(token)),
        }
    }
}

impl<'t> Parse<'t> for ForInitializer {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, Error<'t>> {
        if let Some(initializer) = parser.variable_declaration()? {
            Ok(Some(Self::Declaration(initializer)))
        } else if let Some(expression) = parser.parse()? {
            Ok(Some(Self::Expression(expression)))
        } else {
            Ok(None)
        }
    }
}

pub trait MatchToken<'t>: Sized {
    fn match_token(token: Token<'t>) -> Option<Self>;
}

impl<'t> MatchToken<'t> for OrOperator {
    fn match_token(token: Token) -> Option<Self> {
        (token == Token::Or).then_some(Self)
    }
}

impl<'t> MatchToken<'t> for AndOperator {
    fn match_token(token: Token) -> Option<Self> {
        (token == Token::And).then_some(Self)
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for EqualityOperator {
    fn match_token(token: Token) -> Option<Self> {
        match token {
            Token::BangEqual => Some(Self::NotEqual),
            Token::EqualEqual => Some(Self::Equal),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for ComparisonOperator {
    fn match_token(token: Token) -> Option<Self> {
        match token {
            Token::Greater => Some(Self::Greater),
            Token::GreaterEqual => Some(Self::GreaterEqual),
            Token::Less => Some(Self::Less),
            Token::LessEqual => Some(Self::LessEqual),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for SumOperator {
    fn match_token(token: Token<'t>) -> Option<Self> {
        match token {
            Token::Minus => Some(Self::Minus),
            Token::Plus => Some(Self::Plus),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for FactorOperator {
    fn match_token(token: Token<'t>) -> Option<Self> {
        match token {
            Token::Slash => Some(Self::Divide),
            Token::Star => Some(Self::Multiply),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for UnaryOperator {
    fn match_token(token: Token<'t>) -> Option<Self> {
        match token {
            Token::Minus => Some(Self::Minus),
            Token::Bang => Some(Self::Not),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for Value {
    fn match_token(token: Token<'t>) -> Option<Self> {
        match token {
            Token::Number(number) => Some(Self::Number(number)),
            Token::String(string) => Some(Self::String(string.into())),
            Token::False => Some(Self::Boolean(false)),
            Token::True => Some(Self::Boolean(true)),
            Token::Nil => Some(Self::Nil),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error<'t> {
    UnexpectedToken(SourceToken<'t>),
    UnmatchedParen(usize),
    UnexpectedEndOfInput,
    UnterminatedPrint(usize),
    InvalidLValue(String),
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
            Error::InvalidLValue(string) => write!(f, "Invalid l-value: {string}"),
        }
    }
}

impl error::Error for Error<'_> {}
