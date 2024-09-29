use std::cell::RefCell;
use std::fmt;
use std::sync::Arc;

use thiserror::Error;

use crate::parser::ast::{
    Arithmetic, Binary, Comparison, Equality, Identifier, Literal, Logical, StatementNode,
};

use super::environment::Closure;
use super::{Callable, Clock};

#[derive(Clone, Debug, PartialEq)]
pub struct Value(Arc<RawValue>);

#[derive(Clone, Debug, PartialEq)]
enum RawValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(Function),
    BuiltIn(Clock),
}

impl RawValue {}

impl Value {
    pub fn callable(&self) -> Result<&dyn Callable, TypeError> {
        match self.0.as_ref() {
            RawValue::Number(_) | RawValue::String(_) | RawValue::Boolean(_) | RawValue::Nil => {
                Err(TypeError::NotCallable(self.to_string()))
            }
            RawValue::Function(function) => Ok(function),
            RawValue::BuiltIn(built_in) => Ok(built_in),
        }
    }

    pub fn float(&self) -> Option<f64> {
        match self.0.as_ref() {
            &RawValue::Number(number) => Some(number),
            &RawValue::Boolean(true) => Some(1.0),
            &RawValue::Boolean(false) => Some(0.0),
            RawValue::String(_) | RawValue::Nil | RawValue::BuiltIn(_) | RawValue::Function(_) => {
                None
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self.0.as_ref() {
            &RawValue::Boolean(boolean) => boolean,
            RawValue::Nil => false,
            RawValue::Number(_)
            | RawValue::String(_)
            | RawValue::BuiltIn(_)
            | RawValue::Function(_) => true,
        }
    }

    pub fn function(function: Function) -> Self {
        Self(RawValue::Function(function).into())
    }

    pub fn number(number: f64) -> Self {
        Self(RawValue::Number(number).into())
    }

    pub fn boolean(is_truthy: bool) -> Self {
        Self(RawValue::Boolean(is_truthy).into())
    }

    pub fn nil() -> Self {
        Self(RawValue::Nil.into())
    }

    pub fn built_in(clock: Clock) -> Self {
        Self(RawValue::BuiltIn(clock).into())
    }

    pub fn string(string: String) -> Self {
        Self(RawValue::String(string).into())
    }

    pub fn as_string(&self) -> Option<&str> {
        match self.0.as_ref() {
            RawValue::String(string) => Some(string),
            RawValue::Number(_)
            | RawValue::Boolean(_)
            | RawValue::Nil
            | RawValue::Function(_)
            | RawValue::BuiltIn(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub args: Vec<Identifier>,
    pub body: Box<StatementNode>,
    pub closure: RefCell<Closure>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

impl fmt::Display for RawValue {
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
            Self::Function(function) => write!(f, "{function}"),
            Self::BuiltIn(Clock) => write!(f, "clock"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&Literal> for Value {
    fn from(literal: &Literal) -> Self {
        match literal {
            &Literal::Number(number) => Self(RawValue::Number(number).into()),
            Literal::String(string) => Self(RawValue::String(string.clone()).into()),
            &Literal::Boolean(boolean) => Self(RawValue::Boolean(boolean).into()),
            Literal::Nil => Self(RawValue::Nil.into()),
        }
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Number(number) => Self(RawValue::Number(number).into()),
            Literal::String(string) => Self(RawValue::String(string).into()),
            Literal::Boolean(boolean) => Self(RawValue::Boolean(boolean).into()),
            Literal::Nil => Self(RawValue::Nil.into()),
        }
    }
}

impl Binary {
    pub fn evaluate(self, left: Value, right: Value) -> Result<Value, TypeError> {
        match self {
            Self::Field => todo!(),
            Self::Arithmetic(op) => op.evaluate(&left, &right),
            Self::Comparison(rel) => rel.evaluate(&left, &right).map(Value::boolean),
            Self::Equality(rel) => Ok(Value::boolean(rel.evaluate(&left, &right))),
            Self::Logical(op) => Ok(op.evaluate(left, right)),
        }
    }
}

impl Arithmetic {
    pub fn evaluate(self, left: &Value, right: &Value) -> Result<Value, TypeError> {
        if self == Self::Plus {
            match (left.as_string(), right.as_string()) {
                (Some(left), Some(right)) => return Ok(Value::string(left.to_owned() + right)),
                (Some(_), _) => return Err(TypeError::SumString(right.to_string())),
                (_, Some(_)) => return Err(TypeError::SumString(left.to_string())),
                _ => (),
            }
        }

        let left = self.cast(left)?;
        let right = self.cast(right)?;

        let value = match self {
            Self::Plus => left + right,
            Self::Minus => left - right,
            Self::Times => left * right,
            Self::Divide => left / right,
        };

        Ok(Value::number(value))
    }

    fn cast(self, value: &Value) -> Result<f64, TypeError> {
        value
            .float()
            .ok_or_else(|| TypeError::Arithmetic(self, value.to_string()))
    }
}

impl Logical {
    pub fn evaluate(self, left: Value, right: Value) -> Value {
        match (self, left.is_truthy()) {
            (Self::And, false) | (Self::Or, true) => left,
            (Self::And, true) | (Self::Or, false) => right,
        }
    }

    pub fn short_circuit(self, left: &Value) -> bool {
        match self {
            Self::And => !left.is_truthy(),
            Self::Or => left.is_truthy(),
        }
    }
}

impl Comparison {
    pub fn evaluate(self, left: &Value, right: &Value) -> Result<bool, TypeError> {
        let left = self.cast(left)?;
        let right = self.cast(right)?;

        let value = match self {
            Self::Greater => left > right,
            Self::GreaterEqual => left >= right,
            Self::Less => left < right,
            Self::LessEqual => left <= right,
        };
        Ok(value)
    }

    fn cast(self, value: &Value) -> Result<f64, TypeError> {
        value
            .float()
            .ok_or_else(|| TypeError::Comparison(self, value.to_string()))
    }
}

impl Equality {
    pub fn evaluate(self, left: &Value, right: &Value) -> bool {
        match self {
            Self::Equal => left == right,
            Self::NotEqual => left != right,
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TypeError {
    #[error("cannot compare {1} using {0}, not a number")]
    Comparison(Comparison, String),
    #[error("cannot add {0} to a string")]
    SumString(String),
    #[error("cannot apply {0} to {1}, not a number")]
    Arithmetic(Arithmetic, String),
    #[error("cannot negate {0}")]
    UnaryMinus(String),
    #[error("cannot call {0}")]
    NotCallable(String),
}
