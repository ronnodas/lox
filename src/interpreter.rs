use std::{error, fmt};

use crate::parser::ast::{
    Comparison, Equality, Expression, Factor, Primary, Sum, TypeError, Unary, Value, Visitor,
};

pub struct Interpreter;

impl<'t> Visitor<'t> for Interpreter {
    type Output = Result<Value<'t>, Error<'t>>;

    fn visit_expression(&mut self, expression: &Expression<'t>) -> Self::Output {
        match expression {
            Expression::Equality(equality) => self.visit_equality(equality),
        }
    }

    fn visit_equality(&mut self, equality: &Equality<'t>) -> Self::Output {
        let Equality { start, more } = equality;
        let start = self.visit_comparison(start)?;
        more.iter().try_fold(start, |lhs, (op, rhs)| {
            let rhs = self.visit_comparison(rhs)?;
            Ok(Value::Boolean(op.evaluate(lhs, rhs)))
        })
    }

    fn visit_comparison(&mut self, comparison: &Comparison<'t>) -> Self::Output {
        let Comparison { start, more } = comparison;
        let start = self.visit_sum(start)?;
        more.iter().try_fold(start, |lhs, (op, rhs)| {
            let rhs = self.visit_sum(rhs)?;
            Ok(Value::Boolean(op.evaluate(lhs, rhs)?))
        })
    }

    fn visit_sum(&mut self, sum: &Sum<'t>) -> Self::Output {
        let Sum { start, more } = sum;
        let start = self.visit_factor(start)?;
        more.iter().try_fold(start, |lhs, (op, rhs)| {
            let rhs = self.visit_factor(rhs)?;
            Ok(op.evaluate(&lhs, &rhs)?)
        })
    }

    fn visit_factor(&mut self, factor: &Factor<'t>) -> Self::Output {
        let Factor { start, more } = factor;
        let start = self.visit_unary(start)?;
        more.iter().try_fold(start, |lhs, (op, rhs)| {
            let rhs = self.visit_unary(rhs)?;
            Ok(Value::Number(op.evaluate(lhs, rhs)?))
        })
    }

    fn visit_unary(&mut self, unary: &Unary<'t>) -> Self::Output {
        match unary {
            Unary::Unary(op, unary) => {
                let unary = self.visit_unary(unary)?;
                Ok(op.evaluate(unary)?)
            }
            Unary::Primary(primary) => self.visit_primary(primary),
        }
    }

    fn visit_primary(&mut self, primary: &Primary<'t>) -> Self::Output {
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
