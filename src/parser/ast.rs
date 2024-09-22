use core::fmt;
use std::borrow::Cow;
use std::error;

use crate::tokenizer::Token;

use super::{Error as ParseError, Parser};

pub enum Expression<'t> {
    Equality(Equality<'t>),
}

pub enum Statement<'t> {
    Expression(Expression<'t>),
    Print(Expression<'t>),
}

// TODO: could make the two operands different types, is that useful anywhere?
pub struct Fold<T, Op> {
    pub start: T,
    pub more: Vec<(Op, T)>,
}

pub type Equality<'t> = Fold<Comparison<'t>, EqualityOperator>;
pub type Comparison<'t> = Fold<Sum<'t>, ComparisonOperator>;
pub type Sum<'t> = Fold<Factor<'t>, SumOperator>;
pub type Factor<'t> = Fold<Unary<'t>, FactorOperator>;

#[derive(Clone, Copy)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonOperator {
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl ComparisonOperator {
    pub fn cast(self, value: Value<'_>) -> Result<f64, TypeError<'_>> {
        value.to_float().ok_or(TypeError::Comparison(self, value))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SumOperator {
    Minus,
    Plus,
}

impl SumOperator {
    pub fn cast<'a>(self, lhs: &Value<'a>) -> Result<f64, TypeError<'a>> {
        lhs.to_float()
            .ok_or_else(|| TypeError::Sum(self, lhs.clone()))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FactorOperator {
    Divide,
    Multiply,
}

impl FactorOperator {
    pub fn cast(self, lhs: Value<'_>) -> Result<f64, TypeError<'_>> {
        lhs.to_float().ok_or(TypeError::Factor(self, lhs))
    }
}

pub enum Unary<'t> {
    Unary(UnaryOperator, Box<Self>),
    Primary(Primary<'t>),
}

impl<'t> From<Primary<'t>> for Unary<'t> {
    fn from(v: Primary<'t>) -> Self {
        Self::Primary(v)
    }
}

#[derive(Clone, Copy)]
pub enum UnaryOperator {
    Minus,
    Not,
}

impl UnaryOperator {
    pub(crate) fn evaluate(self, value: Value<'_>) -> Result<Value<'_>, TypeError<'_>> {
        match self {
            Self::Minus => {
                let value = value.to_float().ok_or(TypeError::UnaryMinus(value))?;
                Ok(Value::Number(-value))
            }
            Self::Not => Ok(Value::Boolean(!value.to_boolean())),
        }
    }
}

pub enum Primary<'t> {
    Literal(Value<'t>),
    Grouping(Box<Expression<'t>>),
}

impl<'t> From<Value<'t>> for Primary<'t> {
    fn from(v: Value<'t>) -> Self {
        Self::Literal(v)
    }
}

#[derive(Debug, Clone)]
pub enum Value<'t> {
    Number(f64),
    String(Cow<'t, str>),
    Boolean(bool),
    Nil,
}

impl<'t> From<f64> for Value<'t> {
    fn from(v: f64) -> Self {
        Self::Number(v)
    }
}

impl<'t> From<bool> for Value<'t> {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl<'v> Value<'v> {
    const fn to_float(&self) -> Option<f64> {
        match self {
            &Value::Number(number) => Some(number),
            &Value::Boolean(boolean) => Some(if boolean { 1.0 } else { 0.0 }),
            Value::String(_) | Value::Nil => None,
        }
    }

    const fn to_boolean(&self) -> bool {
        match self {
            &Value::Boolean(boolean) => boolean,
            Value::Nil => false,
            Value::Number(_) | Value::String(_) => true,
        }
    }
}

#[derive(Debug)]
pub enum TypeError<'e> {
    Comparison(ComparisonOperator, Value<'e>),
    Sum(SumOperator, Value<'e>),
    Factor(FactorOperator, Value<'e>),
    UnaryMinus(Value<'e>),
}

impl fmt::Display for TypeError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Comparison(operator, value) => {
                write!(
                    f,
                    "cannot cast {value} to float to compare using {operator}"
                )
            }
            Self::Sum(operator, value) => {
                write!(f, "cannot cast {value} to float to evaluate {operator}")
            }
            Self::Factor(operator, value) => {
                write!(f, "cannot cast {value} to float to evaluate {operator}")
            }
            Self::UnaryMinus(value) => write!(f, "cannot cast {value} to float to negate"),
        }
    }
}

impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
        }
    }
}

impl fmt::Display for SumOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
        }
    }
}

impl fmt::Display for FactorOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Divide => write!(f, "/"),
            Self::Multiply => write!(f, "*"),
        }
    }
}

impl error::Error for TypeError<'_> {}

impl<'t> Primary<'t> {
    pub(crate) fn group(expression: Expression<'t>) -> Self {
        Self::Grouping(Box::new(expression))
    }
}

impl<'t> From<Equality<'t>> for Expression<'t> {
    fn from(v: Equality<'t>) -> Self {
        Self::Equality(v)
    }
}

pub trait MatchToken<'t>: Sized {
    fn match_token(token: Token<'t>) -> Option<Self>;
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
impl<'t> MatchToken<'t> for Value<'t> {
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

pub trait Parse<'t>: Sized {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, ParseError<'t>>;
}

pub trait ParseTower<'t>: Sized {
    type Term: Parse<'t>;
    type Operator: MatchToken<'t>;

    fn new(start: Self::Term, more: Vec<(Self::Operator, Self::Term)>) -> Self;
}

impl<'t> ParseTower<'t> for Equality<'t> {
    type Term = Comparison<'t>;
    type Operator = EqualityOperator;

    fn new(start: Comparison<'t>, more: Vec<(EqualityOperator, Comparison<'t>)>) -> Self {
        Self { start, more }
    }
}

impl<'t> ParseTower<'t> for Comparison<'t> {
    type Term = Sum<'t>;
    type Operator = ComparisonOperator;

    fn new(start: Sum<'t>, more: Vec<(ComparisonOperator, Sum<'t>)>) -> Self {
        Self { start, more }
    }
}

impl<'t> ParseTower<'t> for Sum<'t> {
    type Term = Factor<'t>;
    type Operator = SumOperator;

    fn new(start: Factor<'t>, more: Vec<(SumOperator, Factor<'t>)>) -> Self {
        Self { start, more }
    }
}

impl<'t> ParseTower<'t> for Factor<'t> {
    type Term = Unary<'t>;
    type Operator = FactorOperator;

    fn new(start: Unary<'t>, more: Vec<(FactorOperator, Unary<'t>)>) -> Self {
        Self { start, more }
    }
}

impl<'t, T: ParseTower<'t>> Parse<'t> for T {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, ParseError<'t>> {
        parser.parse_fold()
    }
}

impl<'t> Parse<'t> for Expression<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, ParseError<'t>> {
        parser.expression()
    }
}

impl<'t> Parse<'t> for Unary<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, ParseError<'t>> {
        parser.unary()
    }
}

impl<'t> Parse<'t> for Primary<'t> {
    fn parse(parser: &mut Parser<'_, 't>) -> Result<Option<Self>, ParseError<'t>> {
        parser.literal()
    }
}

pub trait ExpressionVisitor<'t> {
    type Output;
    type Error;

    fn visit_expression(
        &mut self,
        expression: &Expression<'t>,
    ) -> Result<Self::Output, Self::Error>;
    fn visit_unary(&mut self, unary: &Unary<'t>) -> Result<Self::Output, Self::Error>;
    fn visit_primary(&mut self, primary: &Primary<'t>) -> Result<Self::Output, Self::Error>;
    fn visit_equality(&mut self, equality: &Equality<'t>) -> Result<Self::Output, Self::Error>;

    fn visit_fold<T, Op>(&mut self, fold: &Fold<T, Op>) -> Result<Self::Output, Self::Error>
    where
        Self: Sized,
        T: ExpressionHost<'t, Self>,
        Op: BinaryOperator<Self::Output, Output: Into<Self::Output>, Error: Into<Self::Error>>
            + Copy,
    {
        fold.host(self)
    }
}

pub trait StatementVisitor<'t> {
    type Output;
    type Error;

    fn visit_print(&mut self, print: &Expression<'t>) -> Result<Self::Output, Self::Error>;

    fn visit_expression_statement(
        &mut self,
        expression: &Expression<'t>,
    ) -> Result<Self::Output, Self::Error>;

    fn visit_statement(&mut self, statement: &Statement<'t>) -> Result<Self::Output, Self::Error> {
        match statement {
            Statement::Expression(expression) => self.visit_expression_statement(expression),
            Statement::Print(expression) => self.visit_print(expression),
        }
    }
}

pub trait StatementHost<'t, V: StatementVisitor<'t>> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error>;
}

impl<'t, V: StatementVisitor<'t>> StatementHost<'t, V> for Statement<'t> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_statement(self)
    }
}

pub trait BinaryOperator<I> {
    type Output;
    type Error;

    fn evaluate(self, lhs: I, rhs: I) -> Result<Self::Output, Self::Error>;
}

pub trait ExpressionHost<'t, V: ExpressionVisitor<'t>> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error>;
}

impl<'t, V: ExpressionVisitor<'t>> ExpressionHost<'t, V> for Expression<'t> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_expression(self)
    }
}

impl<'t, V: ExpressionVisitor<'t>> ExpressionHost<'t, V> for Unary<'t> {
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        visitor.visit_unary(self)
    }
}

impl<'t, V, T, Op> ExpressionHost<'t, V> for Fold<T, Op>
where
    V: ExpressionVisitor<'t>,
    T: ExpressionHost<'t, V>,
    Op: BinaryOperator<V::Output, Output: Into<V::Output>, Error: Into<V::Error>> + Copy,
{
    fn host(&self, visitor: &mut V) -> Result<V::Output, V::Error> {
        let Self { start, more } = self;
        let start = start.host(visitor)?;
        more.iter().try_fold(start, |lhs, (op, rhs)| {
            let rhs = rhs.host(visitor)?;
            match op.evaluate(lhs, rhs) {
                Ok(output) => Ok(output.into()),
                Err(error) => Err(error.into()),
            }
        })
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(number) => {
                let string = number.to_string();
                if let Some(stripped) = string.strip_suffix(".0") {
                    write!(f, "{stripped}")
                } else {
                    write!(f, "{string}")
                }
            }
            Self::String(string) => write!(f, "\"{string}\""),
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}
