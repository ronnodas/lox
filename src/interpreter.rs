use std::convert::Infallible;
use std::{error, fmt, io};

use anyhow::Context as _;

use crate::parser::ast::{
    BinaryOperator, ComparisonOperator, Equality, EqualityOperator, Expression, ExpressionVisitor,
    FactorOperator, Primary, Statement, StatementHost as _, StatementVisitor, SumOperator,
    TypeError, Unary, Value,
};
use crate::parser::Parser;
use crate::tokenizer::Tokenizer;

pub struct Interpreter;

impl<'t> StatementVisitor<'t> for Interpreter {
    type Output = Option<String>;
    type Error = Error<'t>;

    fn visit_print(&mut self, print: &Expression<'t>) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expression(print)?;
        Ok(Some(format!("{value}")))
    }

    fn visit_expression_statement(
        &mut self,
        expression: &Expression<'t>,
    ) -> Result<Self::Output, Self::Error> {
        _ = self.visit_expression(expression)?;
        Ok(None)
    }
}

impl<'t> ExpressionVisitor<'t> for Interpreter {
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
    pub fn interpret<'s, 'i: 's>(
        &'i mut self,
        statements: Vec<Statement<'s>>,
    ) -> impl Iterator<Item = Result<String, Error<'s>>> + 's {
        statements
            .into_iter()
            .filter_map(|statement| statement.host(self).transpose())
    }

    #[must_use]
    pub fn run<'s, 'i: 's>(
        &'i mut self,
        source: &'s str,
    ) -> Option<impl Iterator<Item = String> + 's> {
        let tokenizer = Tokenizer::new(source);
        let tokens = match tokenizer.into_tokens() {
            Ok(tokens) => tokens,
            Err(e) => {
                eprintln!("Lexing error: {e}");
                return None;
            }
        };
        let parser = Parser::new(&tokens);
        let statements = match parser.into_parsed() {
            Ok(statements) => statements,
            Err(e) => {
                eprintln!("Parsing error: {e}");
                return None;
            }
        };

        Some(
            self.interpret(statements)
                .inspect(|output| {
                    if let Err(e) = output {
                        eprintln!("Runtime error: {e}");
                    }
                })
                .filter_map(Result::ok),
        )
    }

    pub fn run_and_print(&mut self, line: &str) {
        for output in self.run(line).into_iter().flatten() {
            println!("{output}");
        }
    }

    pub fn run_with_prompt(
        &mut self,
        mut output: impl io::Write,
        mut input: impl Iterator<Item = Result<String, impl error::Error + Send + Sync + 'static>>,
    ) -> anyhow::Result<()> {
        loop {
            output.write_all(b"> ")?;
            output.flush()?;
            let Some(line) = input.next() else {
                break;
            };
            let line = line.context("Failed to read line from input")?;

            for print in self.run(&line).into_iter().flatten() {
                output.write_all(print.as_bytes())?;
            }
            output.flush()?;
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn collect_output(&mut self, source: &str) -> Option<Vec<String>> {
        self.run(source).map(Vec::from_iter)
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
