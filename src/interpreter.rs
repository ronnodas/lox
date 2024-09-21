use std::convert::Infallible;
use std::{error, fmt};

use crate::parser::ast::{
    BinaryOperator, Comparison, ComparisonOperator, Equality, EqualityOperator, Expression, Factor,
    FactorOperator, Primary, Sum, SumOperator, TypeError, Unary, Value, Visitor,
};

pub struct Interpreter;

impl<'t> Visitor<'t> for Interpreter {
    type Output = Value<'t>;
    type Error = Error<'t>;

    fn visit_expression(
        &mut self,
        expression: &Expression<'t>,
    ) -> Result<Self::Output, Self::Error> {
        match expression {
            Expression::Equality(equality) => self.visit_equality(equality),
        }
    }

    fn visit_equality(&mut self, equality: &Equality<'t>) -> Result<Self::Output, Self::Error> {
        self.visit_fold(equality)
    }

    fn visit_comparison(
        &mut self,
        comparison: &Comparison<'t>,
    ) -> Result<Self::Output, Self::Error> {
        self.visit_fold(comparison)
    }

    fn visit_sum(&mut self, sum: &Sum<'t>) -> Result<Self::Output, Self::Error> {
        self.visit_fold(sum)
    }

    fn visit_factor(&mut self, factor: &Factor<'t>) -> Result<Self::Output, Self::Error> {
        self.visit_fold(factor)
    }

    fn visit_unary(&mut self, unary: &Unary<'t>) -> Result<Self::Output, Self::Error> {
        match unary {
            Unary::Unary(op, unary) => {
                let unary = self.visit_unary(unary)?;
                Ok(op.evaluate(unary)?)
            }
            Unary::Primary(primary) => self.visit_primary(primary),
        }
    }

    fn visit_primary(&mut self, primary: &Primary<'t>) -> Result<Self::Output, Self::Error> {
        match primary {
            Primary::Literal(value) => Ok(value.clone()),
            Primary::Grouping(expression) => self.visit_expression(expression),
        }
    }
}

impl Interpreter {
    pub fn interpret<'e>(&mut self, expression: &Expression<'e>) -> Result<(), Error<'e>> {
        println!("{}", self.visit_expression(expression)?);
        Ok(())
    }
}

#[derive(Debug)]
pub enum Error<'e> {
    Type(TypeError<'e>),
}

impl<'e> From<Infallible> for Error<'e> {
    fn from(infallible: Infallible) -> Self {
        match infallible {}
    }
}

impl<'e> From<TypeError<'e>> for Error<'e> {
    fn from(v: TypeError<'e>) -> Self {
        Self::Type(v)
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Type(error) => write!(f, "Type error: {error}"),
        }
    }
}

impl error::Error for Error<'_> {}

impl<'t> BinaryOperator<Value<'t>> for EqualityOperator {
    type Output = bool;
    type Error = Infallible;

    #[expect(clippy::float_cmp, reason = "Lox spec is weird")]
    fn evaluate(self, lhs: Value<'t>, rhs: Value<'t>) -> Result<bool, Self::Error> {
        let equal = match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        };
        match self {
            Self::Equal => Ok(equal),
            Self::NotEqual => Ok(!equal),
        }
    }
}

impl<'t> BinaryOperator<Value<'t>> for ComparisonOperator {
    type Output = bool;
    type Error = TypeError<'t>;

    fn evaluate(self, lhs: Value<'t>, rhs: Value<'t>) -> Result<bool, Self::Error> {
        let lhs = self.cast(lhs)?;
        let rhs = self.cast(rhs)?;
        Ok(match self {
            Self::Greater => lhs > rhs,
            Self::GreaterEqual => lhs >= rhs,
            Self::Less => lhs < rhs,
            Self::LessEqual => lhs <= rhs,
        })
    }
}

impl<'t> BinaryOperator<Value<'t>> for SumOperator {
    type Output = Value<'t>;
    type Error = TypeError<'t>;

    fn evaluate(self, lhs: Value<'t>, rhs: Value<'t>) -> Result<Self::Output, Self::Error> {
        if let (Value::String(lhs), Self::Plus, Value::String(rhs)) = (&lhs, self, &rhs) {
            let string = lhs.as_ref().to_owned() + rhs.as_ref();
            return Ok(Value::String(string.into()));
        }
        let lhs = self.cast(&lhs)?;
        let rhs = self.cast(&rhs)?;
        Ok(match self {
            Self::Minus => Value::Number(lhs - rhs),
            Self::Plus => Value::Number(lhs + rhs),
        })
    }
}

impl<'t> BinaryOperator<Value<'t>> for FactorOperator {
    type Output = f64;
    type Error = TypeError<'t>;

    fn evaluate(self, lhs: Value<'t>, rhs: Value<'t>) -> Result<f64, Self::Error> {
        let lhs = self.cast(lhs)?;
        let rhs = self.cast(rhs)?;
        Ok(match self {
            Self::Divide => lhs / rhs,
            Self::Multiply => lhs * rhs,
        })
    }
}
