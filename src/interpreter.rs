mod environment;

use std::convert::Infallible;
use std::{error, fmt, io};

use anyhow::Context as _;
use itertools::Itertools as _;

use crate::parser::ast::{
    Assignment, BinaryOperator, ComparisonOperator, Declaration, EqualityOperator, Expression,
    ExpressionHost, ExpressionVisitor, FactorOperator, Identifier, Primary, Statement,
    StatementHost as _, StatementVisitor, SumOperator, TypeError, Unary, Value,
};
use crate::parser::{Error as ParseError, Parser};
use crate::tokenizer::{Error as TokenError, Tokenizer};
use environment::Environment;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl StatementVisitor for Interpreter {
    type Output = Vec<String>;
    type Error = RuntimeError;

    fn visit_print(&mut self, print: &Expression) -> Result<Self::Output, Self::Error> {
        let value = self.visit_expression(print)?;
        Ok(vec![format!("{value}")])
    }

    fn visit_expression_statement(
        &mut self,
        expression: &Expression,
    ) -> Result<Self::Output, Self::Error> {
        _ = self.visit_expression(expression)?;
        Ok(vec![])
    }

    fn visit_variable_declaration(
        &mut self,
        identifier: &str,
        initializer: Option<&Expression>,
    ) -> Result<Self::Output, Self::Error> {
        let value = initializer
            .map(|expression| self.visit_expression(expression))
            .transpose()?;
        self.environment.declare(identifier, value);
        Ok(vec![])
    }

    fn visit_block(&mut self, block: &[Declaration]) -> Result<Self::Output, Self::Error> {
        self.environment.push();
        let output = block
            .iter()
            .map(|declaration| declaration.host(self))
            .flatten_ok()
            .collect();
        self.environment.pop();
        output
    }

    fn visit_if(
        &mut self,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> Result<Self::Output, Self::Error> {
        let condition = self.visit_expression(condition)?;
        if condition.is_truthy() {
            then_branch.host(self)
        } else if let Some(else_branch) = else_branch {
            else_branch.host(self)
        } else {
            Ok(vec![])
        }
    }
}

impl ExpressionVisitor for Interpreter {
    type Output = Value;
    type Error = RuntimeError;

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
                .ok_or_else(|| RuntimeError::UndeclaredVariable(Identifier::clone(identifier))),
        }
    }

    fn visit_assignment(&mut self, assignment: &Assignment) -> Result<Self::Output, Self::Error> {
        let value = assignment.expression.host(self)?;
        let Some(()) = self.environment.set(&assignment.lvalue, value.clone()) else {
            let identifier = Identifier::clone(&assignment.lvalue);
            return Err(RuntimeError::UndeclaredVariable(identifier));
        };
        Ok(value)
    }
}

impl Interpreter {
    pub fn interpret<'i>(
        &'i mut self,
        statements: impl IntoIterator<Item = Declaration> + 'i,
    ) -> Result<Vec<String>, RuntimeError> {
        statements
            .into_iter()
            .map(|declaration| declaration.host(self))
            .flatten_ok()
            .collect()
    }

    pub fn run<'s>(&mut self, source: &'s str) -> Result<Vec<String>, Error<'s>> {
        let tokens = Tokenizer::new(source).into_tokens()?;
        let statements: Vec<Declaration> = Parser::new(&tokens).into_statements().try_collect()?;

        Ok(self.interpret(statements)?)
    }

    pub fn run_and_print(&mut self, line: &str) {
        match self.run(line) {
            Ok(output) => {
                for output in output {
                    println!("{output}");
                }
            }
            Err(e) => eprintln!("{e:?}"),
        }
    }

    pub fn run_with_prompt(
        &mut self,
        mut output: impl io::Write,
        mut input: impl Iterator<Item = Result<String, impl error::Error + Send + Sync + 'static>>,
    ) -> anyhow::Result<()> {
        // TODO QoL:
        // * implicit print
        // * add semicolons
        // * multiline input

        loop {
            output.write_all(b"> ")?;
            output.flush()?;
            let Some(line) = input.next() else {
                break;
            };
            let line = line.context("Failed to read line from input")?;

            match self.run(&line) {
                Ok(print) => {
                    for print in print {
                        output.write_all(print.as_bytes())?;
                        output.write_all(b"\n")?;
                    }
                    output.flush()?;
                }
                Err(e) => eprintln!("{e:?}"),
            }
        }
        Ok(())
    }

    pub fn new() -> Self {
        Self {
            environment: Environment::default(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error<'t> {
    Tokenizing(TokenError),
    Parsing(ParseError<'t>),
    Runtime(RuntimeError),
}

impl<'t> From<RuntimeError> for Error<'t> {
    fn from(v: RuntimeError) -> Self {
        Self::Runtime(v)
    }
}

impl<'t> From<ParseError<'t>> for Error<'t> {
    fn from(v: ParseError<'t>) -> Self {
        Self::Parsing(v)
    }
}

impl<'t> From<TokenError> for Error<'t> {
    fn from(v: TokenError) -> Self {
        Self::Tokenizing(v)
    }
}

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    Type(TypeError),
    UndeclaredVariable(Identifier),
}

impl From<Infallible> for RuntimeError {
    fn from(infallible: Infallible) -> Self {
        match infallible {}
    }
}

impl From<TypeError> for RuntimeError {
    fn from(v: TypeError) -> Self {
        Self::Type(v)
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(error) => write!(f, "Type error: {error}"),
            Self::UndeclaredVariable(identifer) => write!(f, "Undeclared variable: {identifer}"),
        }
    }
}

impl error::Error for RuntimeError {}

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
