use core::fmt;
use std::borrow::Cow;
use std::error;

use crate::tokenizer::Token;

use super::{Error as ParseError, Parser};

pub enum Expression<'t> {
    Equality(Equality<'t>),
}

pub struct Equality<'t> {
    pub start: Comparison<'t>,
    pub more: Vec<(EqualityOperator, Comparison<'t>)>,
}

#[derive(Clone, Copy)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

#[expect(clippy::float_cmp, reason = "Lox spec is weird")]
impl EqualityOperator {
    pub fn evaluate<'t>(self, lhs: Value<'t>, rhs: Value<'t>) -> bool {
        let equal = match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        };
        match self {
            Self::Equal => equal,
            Self::NotEqual => !equal,
        }
    }
}

pub struct Comparison<'t> {
    pub start: Sum<'t>,
    pub more: Vec<(ComparisonOperator, Sum<'t>)>,
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonOperator {
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl ComparisonOperator {
    pub fn evaluate<'e>(self, lhs: Value<'e>, rhs: Value<'e>) -> Result<bool, TypeError<'e>> {
        let lhs = self.cast(lhs)?;
        let rhs = self.cast(rhs)?;
        Ok(match self {
            Self::Greater => lhs > rhs,
            Self::GreaterEqual => lhs >= rhs,
            Self::Less => lhs < rhs,
            Self::LessEqual => lhs <= rhs,
        })
    }

    fn cast(self, value: Value<'_>) -> Result<f64, TypeError<'_>> {
        value.to_float().ok_or(TypeError::Comparison(self, value))
    }
}

pub struct Sum<'t> {
    pub start: Factor<'t>,
    pub more: Vec<(SumOperator, Factor<'t>)>,
}

#[derive(Debug, Clone, Copy)]
pub enum SumOperator {
    Minus,
    Plus,
}

impl SumOperator {
    pub fn evaluate<'v>(
        self,
        lhs: &Value<'v>,
        rhs: &Value<'v>,
    ) -> Result<Value<'v>, TypeError<'v>> {
        if let (Value::String(lhs), Self::Plus, Value::String(rhs)) = (lhs, self, rhs) {
            let string = lhs.as_ref().to_owned() + rhs.as_ref();
            return Ok(Value::String(string.into()));
        }
        let lhs = self.cast(lhs)?;
        let rhs = self.cast(rhs)?;
        Ok(match self {
            Self::Minus => Value::Number(lhs - rhs),
            Self::Plus => Value::Number(lhs + rhs),
        })
    }

    fn cast<'a>(self, lhs: &Value<'a>) -> Result<f64, TypeError<'a>> {
        lhs.to_float()
            .ok_or_else(|| TypeError::Sum(self, lhs.clone()))
    }
}

pub struct Factor<'t> {
    pub start: Unary<'t>,
    pub more: Vec<(FactorOperator, Unary<'t>)>,
}

#[derive(Debug, Clone, Copy)]
pub enum FactorOperator {
    Divide,
    Multiply,
}

impl FactorOperator {
    pub fn evaluate<'v>(self, lhs: Value<'v>, rhs: Value<'v>) -> Result<f64, TypeError<'v>> {
        let lhs = self.cast(lhs)?;
        let rhs = self.cast(rhs)?;
        Ok(match self {
            Self::Divide => lhs / rhs,
            Self::Multiply => lhs * rhs,
        })
    }

    fn cast(self, lhs: Value<'_>) -> Result<f64, TypeError<'_>> {
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
        parser.parse_tower()
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

pub trait Visitor<'t> {
    type Output;

    fn visit_expression(&mut self, expression: &Expression<'t>) -> Self::Output;
    fn visit_equality(&mut self, equality: &Equality<'t>) -> Self::Output;
    fn visit_comparison(&mut self, comparison: &Comparison<'t>) -> Self::Output;
    fn visit_sum(&mut self, sum: &Sum<'t>) -> Self::Output;
    fn visit_factor(&mut self, factor: &Factor<'t>) -> Self::Output;
    fn visit_unary(&mut self, unary: &Unary<'t>) -> Self::Output;
    fn visit_primary(&mut self, primary: &Primary<'t>) -> Self::Output;
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
