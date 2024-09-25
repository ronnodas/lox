pub mod ast;
pub mod printer;

use std::iter::from_fn;

use ast::{
    AndOperator, Assignment, ComparisonOperator, Declaration, EqualityOperator, Expression,
    FactorOperator, Fold, Identifier, IntoLValue as _, LogicalOr, OrOperator, Primary, Statement,
    SumOperator, Unary, UnaryOperator, Value,
};
use itertools::Itertools as _;
use miette::Diagnostic;
use thiserror::Error;

use crate::tokenizer::{SourceSpan, Token, TokenKind};

type WithSpanResult<T> = Result<Option<(T, SourceSpan)>, ParseError>;

pub struct Parser<'p, 't> {
    tokens: &'p [Token<'t>],
}

impl<'t, 'p> Parser<'p, 't> {
    pub const fn new(tokens: &'p [Token<'t>]) -> Self {
        Self { tokens }
    }

    pub fn into_statements(mut self) -> impl Iterator<Item = Result<Declaration, ParseError>> + 'p {
        from_fn(move || {
            let statement = self.parse().transpose()?;
            if statement.is_err() {
                self.recover();
            }
            Some(statement)
        })
    }

    fn parse<T: Parse<'t>>(&mut self) -> Result<Option<T>, ParseError> {
        T::parse(self)
    }

    fn with_span<T: Parse<'t>>(&mut self) -> WithSpanResult<T> {
        T::with_span(self)
    }

    fn print_statement(&mut self) -> WithSpanResult<Statement> {
        let Ok(print) = self.consume(&TokenKind::Print) else {
            return Ok(None);
        };
        let Some(expression) = self.parse()? else {
            return Err(self.unexpected());
        };

        let Ok(semicolon) = self.consume(&TokenKind::Semicolon) else {
            return Err(WithSpan::from_token(ErrorKind::UnterminatedPrint, &print).into());
        };

        let print_statement = Statement::Print(expression);
        Ok(Some((print_statement, print.span | semicolon.span)))
    }

    fn expression_statement(&mut self) -> WithSpanResult<Statement> {
        let Some((expression, span)) = self.with_span()? else {
            return Ok(None);
        };
        let span = span | self.consume(&TokenKind::Semicolon)?.span;
        Ok(Some((Statement::Expression(expression), span)))
    }

    fn block(&mut self) -> WithSpanResult<Statement> {
        if let Ok(left) = self.consume(&TokenKind::LeftBrace) {
            let declarations = from_fn(|| self.parse().transpose()).try_collect()?;
            let span = left.span | self.consume(&TokenKind::RightBrace)?.span;
            Ok(Some((Statement::Block(declarations), span)))
        } else {
            Ok(None)
        }
    }

    fn unexpected(&self) -> ParseError {
        self.tokens
            .first()
            .map_or(ParseError::UnexpectedEndOfInput, |token| {
                WithSpan::from_token(ErrorKind::UnexpectedToken, token).into()
            })
    }

    fn consume(&mut self, kind: &TokenKind) -> Result<Token<'t>, ParseError> {
        if let Some((&next, rest)) = self.tokens.split_first() {
            if &next.kind == kind {
                self.tokens = rest;
                Ok(next)
            } else {
                Err(WithSpan::from_token(ErrorKind::UnexpectedToken, &next).into())
            }
        } else {
            Err(ParseError::UnexpectedEndOfInput)
        }
    }

    fn consume_identifier(&mut self) -> Option<(Identifier, SourceSpan)> {
        if let (
            &Token {
                kind: TokenKind::Identifier(identifier),
                span,
            },
            rest,
        ) = self.tokens.split_first()?
        {
            self.tokens = rest;
            Some((identifier.into(), span))
        } else {
            None
        }
    }

    fn consume_match<T: MatchToken<'t>>(&mut self) -> Option<T> {
        self.consume_match_with_span().map(|(t, _)| t)
    }

    fn consume_match_with_span<T: MatchToken<'t>>(&mut self) -> Option<(T, SourceSpan)> {
        let (next, rest) = self.tokens.split_first()?;
        let span = next.span;
        if let Some(next) = T::match_token(next.kind) {
            self.tokens = rest;
            Some((next, span))
        } else {
            None
        }
    }

    fn recover(&mut self) {
        let mid = if let Some(mid) = self
            .tokens
            .iter()
            .position(|t| t.kind == TokenKind::Semicolon)
        {
            mid
        } else {
            self.tokens.len()
        };
        self.tokens = &self.tokens[mid..];
    }

    fn variable_declaration(&mut self) -> WithSpanResult<Declaration> {
        let span = match self.consume(&TokenKind::Var) {
            Ok(token) => token.span,
            Err(_) => return Ok(None),
        };
        let Some(Primary::Identifier(identifier)) = self.parse()? else {
            return Err(self.unexpected());
        };
        let initializer = if self.consume(&TokenKind::Equal).is_ok() {
            self.parse()?
        } else {
            None
        };
        let span = span | self.consume(&TokenKind::Semicolon)?.span;
        let declaration = Declaration::VariableDeclaration {
            identifier,
            initializer,
        };
        Ok(Some((declaration, span)))
    }

    fn if_statement(&mut self) -> WithSpanResult<Statement> {
        let start = match self.consume(&TokenKind::If) {
            Ok(token) => token.span,
            Err(_) => return Ok(None),
        };
        _ = self.consume(&TokenKind::LeftParen)?;
        let Some(condition) = self.parse()? else {
            return Err(self.unexpected());
        };
        _ = self.consume(&TokenKind::RightParen)?;
        let Some((then_branch, mut end)) = self.with_span()? else {
            return Err(self.unexpected());
        };
        let else_branch = if self.consume(&TokenKind::Else).is_ok() {
            if let Some((else_branch, else_end)) = self.with_span()? {
                end = else_end;
                Some(Box::new(else_branch))
            } else {
                return Err(self.unexpected());
            }
        } else {
            None
        };

        let statement = Statement::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        };
        Ok(Some((statement, start | end)))
    }

    fn while_statement(&mut self) -> WithSpanResult<Statement> {
        let start = match self.consume(&TokenKind::While) {
            Ok(token) => token.span,
            Err(_) => return Ok(None),
        };
        _ = self.consume(&TokenKind::LeftParen)?;
        let Some(condition) = self.parse()? else {
            return Err(self.unexpected());
        };
        _ = self.consume(&TokenKind::RightParen)?;
        let Some((body, end)) = self.with_span()? else {
            return Err(self.unexpected());
        };
        let statement = Statement::While {
            condition,
            body: Box::new(body),
        };
        Ok(Some((statement, start | end)))
    }

    fn for_statement(&mut self) -> WithSpanResult<Statement> {
        let start = match self.consume(&TokenKind::For) {
            Ok(token) => token.span,
            Err(_) => return Ok(None),
        };
        _ = self.consume(&TokenKind::LeftParen)?;
        let initializer = self.parse()?;
        if initializer.is_none() {
            _ = self.consume(&TokenKind::Semicolon)?;
        }
        let condition = self.parse()?;
        _ = self.consume(&TokenKind::Semicolon)?;
        let increment = self.parse()?;
        _ = self.consume(&TokenKind::RightParen)?;
        let Some((body, end)) = self.with_span()? else {
            return Err(self.unexpected());
        };
        let for_statement = desugar_for(initializer, condition, increment, body);
        Ok(Some((for_statement, start | end)))
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
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self>;

    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, ParseError> {
        Self::with_span(parser).map(|value| value.map(|(value, _)| value))
    }
}

//TODO maybe inline and restructure once all the different statements are implemented
impl<'t> Parse<'t> for Declaration {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        if let declaration @ Some(_) = parser.variable_declaration()? {
            Ok(declaration)
        } else {
            Ok(parser
                .with_span()?
                .map(|(statement, span)| (Self::Statement(statement), span)))
        }
    }
}

impl<'t> Parse<'t> for Statement {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
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
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        let Some((expression, span)) = parser.with_span::<LogicalOr>()? else {
            return Ok(None);
        };
        if parser.consume(&TokenKind::Equal).is_err() {
            return Ok(Some((Self::Or(expression), span)));
        };
        let Some(lvalue) = expression.lvalue() else {
            return Err(WithSpan::new(ErrorKind::InvalidLValue, span).into());
        };
        let Some((expression, end)) = parser.with_span()? else {
            return Err(parser.unexpected());
        };
        let assignment = Self::Assignment(Assignment {
            lvalue,
            expression: Box::new(expression),
        });
        Ok(Some((assignment, span | end)))
    }
}

impl<'t, T: Parse<'t>, Op: MatchToken<'t>> Parse<'t> for Fold<T, Op> {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        let this = &mut *parser;
        let Some((start, mut span)) = this.with_span()? else {
            return Ok(None);
        };
        let mut more = Vec::new();
        while let Some(operator) = this.consume_match() {
            let Some((right, right_span)) = this.with_span()? else {
                return Err(this.unexpected());
            };
            more.push((operator, right));
            span = span | right_span;
        }
        Ok(Some((Self { start, more }, span)))
    }
}

impl<'t> Parse<'t> for Unary {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        if let Some((operator, span)) = parser.consume_match_with_span() {
            let Some((right, right_span)) = parser.with_span()? else {
                return Err(parser.unexpected());
            };
            let unary = Self::Unary(operator, Box::new(right));
            Ok(Some((unary, span | right_span)))
        } else {
            parser
                .with_span()
                .map(|e| e.map(|(e, span)| (Self::Primary(e), span)))
        }
    }
}

impl<'t> Parse<'t> for Primary {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        if let Some((value, span)) = parser.with_span()? {
            return Ok(Some((Self::Literal(value), span)));
        }
        if let Some((identifier, span)) = parser.consume_identifier() {
            return Ok(Some((Self::Identifier(identifier), span)));
        }
        let Ok(left) = parser.consume(&TokenKind::LeftParen) else {
            return Ok(None);
        };
        let Some(expression) = parser.parse()? else {
            let error = match parser.tokens.first() {
                Some(&token) => WithSpan::from_token(ErrorKind::UnexpectedToken, &token),
                None => WithSpan::from_token(ErrorKind::UnmatchedParen, &left),
            };
            return Err(error.into());
        };
        match parser.consume(&TokenKind::RightParen) {
            Ok(right) => Ok(Some((Self::group(expression), left.span | right.span))),
            Err(ParseError::UnexpectedEndOfInput) => {
                Err(WithSpan::from_token(ErrorKind::UnmatchedParen, &left).into())
            }
            Err(e) => Err(e),
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> Parse<'t> for Value {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        let Some((token, rest)) = parser.tokens.split_first() else {
            return Ok(None);
        };
        let value = match token.kind {
            TokenKind::Number(number) => Self::Number(number),
            TokenKind::False => Self::Boolean(false),
            TokenKind::True => Self::Boolean(true),
            TokenKind::Nil => Self::Nil,
            TokenKind::String(string) => {
                Self::String(string.replace(r#"\""#, "\"").replace("\\\\", "\\").into())
            }
            _ => return Ok(None),
        };
        parser.tokens = rest;
        Ok(Some((value, token.span)))
    }
}

impl<'t> Parse<'t> for ForInitializer {
    fn with_span(parser: &mut Parser<'_, 't>) -> WithSpanResult<Self> {
        if let Some((initializer, span)) = parser.variable_declaration()? {
            Ok(Some((Self::Declaration(initializer), span)))
        } else if let Some((expression, span)) = parser.with_span()? {
            Ok(Some((Self::Expression(expression), span)))
        } else {
            Ok(None)
        }
    }
}

pub trait MatchToken<'t>: Sized {
    fn match_token(token: TokenKind) -> Option<Self>;
}

impl<'t> MatchToken<'t> for OrOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        (token == TokenKind::Or).then_some(Self)
    }
}

impl<'t> MatchToken<'t> for AndOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        (token == TokenKind::And).then_some(Self)
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for EqualityOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        match token {
            TokenKind::BangEqual => Some(Self::NotEqual),
            TokenKind::EqualEqual => Some(Self::Equal),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for ComparisonOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        match token {
            TokenKind::Greater => Some(Self::Greater),
            TokenKind::GreaterEqual => Some(Self::GreaterEqual),
            TokenKind::Less => Some(Self::Less),
            TokenKind::LessEqual => Some(Self::LessEqual),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for SumOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        match token {
            TokenKind::Minus => Some(Self::Minus),
            TokenKind::Plus => Some(Self::Plus),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for FactorOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        match token {
            TokenKind::Slash => Some(Self::Divide),
            TokenKind::Star => Some(Self::Multiply),
            _ => None,
        }
    }
}

#[expect(clippy::wildcard_enum_match_arm, reason = "noise")]
impl<'t> MatchToken<'t> for UnaryOperator {
    fn match_token(token: TokenKind) -> Option<Self> {
        match token {
            TokenKind::Minus => Some(Self::Minus),
            TokenKind::Bang => Some(Self::Not),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Error, Diagnostic, Clone, Copy)]
pub enum ParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    WithSpan(#[from] WithSpan),
    // TODO support batching
    #[error("Input ended before parsing finished")]
    UnexpectedEndOfInput,
}

#[derive(Debug, PartialEq, Eq, Error, Diagnostic, Clone, Copy)]
#[error("{}", self.kind)]
#[diagnostic()]
pub struct WithSpan {
    kind: ErrorKind,
    #[label("{}", self.kind.label())]
    span: SourceSpan,
}

impl WithSpan {
    const fn new(kind: ErrorKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    const fn from_token(kind: ErrorKind, token: &Token<'_>) -> Self {
        Self {
            kind,
            span: token.span,
        }
    }
}

//TODO add expected token to UnexpectedToken, probably want to pull in strum
#[derive(Error, Debug, Diagnostic, Clone, Copy, PartialEq, Eq)]
enum ErrorKind {
    #[error("Unexpected token while parsing")]
    UnexpectedToken,
    #[error("Unmatched parenthesis")]
    UnmatchedParen,
    #[error("Unterminated print statement")]
    UnterminatedPrint,
    #[error("Invalid l-value")]
    InvalidLValue,
}

impl ErrorKind {
    const fn label(self) -> &'static str {
        match self {
            Self::UnexpectedToken => "this token",
            Self::UnmatchedParen => "this open parenthesis",
            Self::UnterminatedPrint => "print starts here",
            Self::InvalidLValue => "this expression cannot be assigned to",
        }
    }
}
