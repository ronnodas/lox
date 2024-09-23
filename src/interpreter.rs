mod environment;

use std::convert::Infallible;
use std::{error, fmt, io};

use anyhow::Context as _;

use crate::parser::ast::{
    Assignment, BinaryOperator, ComparisonOperator, Declaration, EqualityOperator, Expression,
    ExpressionHost, ExpressionVisitor, FactorOperator, Identifier, Primary, StatementHost as _,
    StatementVisitor, SumOperator, TypeError, Unary, Value,
};
use crate::parser::Parser;
use crate::tokenizer::Tokenizer;
use environment::Environment;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl StatementVisitor for Interpreter {
    type Output = Option<String>;
    type Error = Error;

    fn visit_print(&mut self, print: &Expression) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expression(print)?;
        Ok(Some(format!("{value}")))
    }

    fn visit_expression_statement(
        &mut self,
        expression: &Expression,
    ) -> Result<Self::Output, Self::Error> {
        _ = self.visit_expression(expression)?;
        Ok(None)
    }

    fn visit_variable_declaration(
        &mut self,
        identifier: &str,
        initializer: Option<&Expression>,
    ) -> Result<Self::Output, Self::Error> {
        let value = initializer
            .map(|expression| self.visit_expression(expression))
            .transpose()?
            .unwrap_or(Value::Nil);
        self.environment.set(identifier, value);
        Ok(None)
    }
}

impl ExpressionVisitor for Interpreter {
    type Output = Value;
    type Error = Error;

    fn visit_expression(&mut self, expression: &Expression) -> Result<Self::Output, Self::Error> {
        match expression {
            Expression::Assignment(assignment) => assignment.host(self),
            Expression::Equality(equality) => equality.host(self),
        }
    }

    fn visit_unary(&mut self, unary: &Unary) -> Result<Self::Output, Self::Error> {
        match unary {
            Unary::Unary(op, unary) => {
                let unary = self.visit_unary(unary)?;
                Ok(op.evaluate(unary)?)
            }
            Unary::Primary(primary) => self.visit_primary(primary),
        }
    }

    fn visit_primary(&mut self, primary: &Primary) -> Result<Self::Output, Self::Error> {
        match primary {
            Primary::Literal(value) => Ok(value.clone()),
            Primary::Grouping(expression) => self.visit_expression(expression),
            Primary::Identifier(identifier) => self
                .environment
                .get(identifier)
                .ok_or_else(|| Error::UndeclaredVariable(Identifier::clone(identifier))),
        }
    }

    fn visit_assignment(&mut self, assignment: &Assignment) -> Result<Self::Output, Self::Error> {
        let value = assignment.expression.host(self)?;
        self.environment.set(&assignment.lvalue, value.clone());
        Ok(value)
    }
}

impl Interpreter {
    pub fn interpret<'i>(
        &'i mut self,
        statements: impl IntoIterator<Item = Declaration> + 'i,
    ) -> impl Iterator<Item = Result<String, Error>> + 'i {
        statements
            .into_iter()
            .filter_map(|declaration| declaration.host(self).transpose())
    }

    #[must_use]
    pub fn run<'i>(&'i mut self, source: &str) -> Option<impl Iterator<Item = String> + 'i> {
        let tokens = Tokenizer::new(source)
            .into_tokens()
            .inspect_err(|e| eprintln!("Lexing error: {e}"))
            .ok()?;
        let statements: Vec<_> = Parser::new(&tokens)
            .into_statements()
            .filter_map(|statement| {
                statement
                    .inspect_err(|e| eprintln!("Parsing error: {e}"))
                    .ok()
            })
            .collect();

        Some(
            self.interpret(statements)
                .filter_map(|output| output.inspect_err(|e| eprintln!("Runtime error: {e}")).ok()),
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

    pub fn new() -> Self {
        Self {
            environment: Environment::default(),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Type(TypeError),
    UndeclaredVariable(Identifier),
}

impl From<Infallible> for Error {
    fn from(infallible: Infallible) -> Self {
        match infallible {}
    }
}

impl From<TypeError> for Error {
    fn from(v: TypeError) -> Self {
        Self::Type(v)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(error) => write!(f, "Type error: {error}"),
            Self::UndeclaredVariable(identifer) => write!(f, "Undeclared variable: {identifer}"),
        }
    }
}

impl error::Error for Error {}

impl BinaryOperator<Value> for EqualityOperator {
    type Output = bool;
    type Error = Infallible;

    #[expect(clippy::float_cmp, reason = "Lox spec is weird")]
    fn evaluate(self, lhs: Value, rhs: Value) -> Result<bool, Self::Error> {
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

impl BinaryOperator<Value> for ComparisonOperator {
    type Output = bool;
    type Error = TypeError;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<bool, Self::Error> {
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

impl BinaryOperator<Value> for SumOperator {
    type Output = Value;
    type Error = TypeError;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<Self::Output, Self::Error> {
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

impl BinaryOperator<Value> for FactorOperator {
    type Output = f64;
    type Error = TypeError;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<f64, Self::Error> {
        let lhs = self.cast(lhs)?;
        let rhs = self.cast(rhs)?;
        Ok(match self {
            Self::Divide => lhs / rhs,
            Self::Multiply => lhs * rhs,
        })
    }
}
