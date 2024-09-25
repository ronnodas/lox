mod environment;

use std::convert::Infallible;
use std::{fmt, io};

use itertools::Itertools as _;
use miette::Diagnostic;
use thiserror::Error;

use crate::parser::ast::{
    AndOperator, Assignment, Comparison, ComparisonOperator, Declaration, Equality,
    EqualityOperator, Expression, ExpressionHost, ExpressionVisitCombiner, ExpressionVisitor,
    Factor, FactorOperator, Identifier, LogicalAnd, OrOperator, Primary, Statement,
    StatementHost as _, StatementVisitor, Sum, SumOperator, TypeError, Unary, UnaryOperator, Value,
};
use crate::parser::Parser;
use crate::tokenizer::Tokenizer;
use crate::LoxError;
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

    fn visit_while(
        &mut self,
        condition: &Expression,
        body: &Statement,
    ) -> Result<Self::Output, Self::Error> {
        let mut outputs = vec![];
        while condition.host(self)?.is_truthy() {
            outputs.extend(body.host(self)?);
        }
        Ok(outputs)
    }
}

impl ExpressionVisitor for Interpreter {
    type Output = Value;
    type Error = RuntimeError;

    fn visit_expression(&mut self, expression: &Expression) -> Result<Self::Output, Self::Error> {
        match expression {
            Expression::Assignment(assignment) => assignment.host(self),
            Expression::Or(or) => or.host(self),
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

    pub fn run(&mut self, source: &str) -> Result<Vec<String>, LoxError> {
        let tokens: Vec<_> = Tokenizer::new(source).try_collect()?;
        let statements: Vec<Declaration> = Parser::new(&tokens)
            .into_statements()
            .try_collect()
            .map_err(LoxError::Parsing)?;

        Ok(self.interpret(statements)?)
    }

    pub fn run_and_print(&mut self, line: &str) {
        match self.run(line) {
            Ok(output) => {
                for output in output {
                    println!("{output}");
                }
            }
            Err(e) => eprintln!(
                "{:?}",
                miette::Report::from(e).with_source_code(line.to_owned())
            ),
        }
    }

    pub fn run_with_prompt(
        &mut self,
        mut output: impl io::Write,
        mut input: impl Iterator<Item = Result<String, impl Into<LoxError>>>,
    ) -> Result<(), LoxError> {
        // TODO QoL:
        // * implicit print
        // * multiline input
        // * multiline output
        // * arrow keys

        loop {
            output.write_all(b"> ")?;
            output.flush()?;
            let Some(line) = input.next() else {
                break;
            };
            let line = line.map_err(Into::into)? + ";";

            match self.run(&line) {
                Ok(print) => {
                    for print in print {
                        output.write_all(print.as_bytes())?;
                        output.write_all(b"\n")?;
                    }
                    output.flush()?;
                }
                Err(e) => eprintln!("{:?}", miette::Error::from(e).with_source_code(line)),
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

#[derive(Debug, Diagnostic, Error, PartialEq)]
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

pub trait BinaryOperator<T: ExpressionHost<Interpreter>>: Copy {
    type Output: Into<Value>;
    type Error: Into<RuntimeError>;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<Self::Output, Self::Error>;

    fn eager_combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &T,
    ) -> Result<Value, RuntimeError> {
        let rhs = rhs.host(visitor)?;
        self.evaluate(lhs, rhs)
            .map_err(Self::Error::into)
            .map(Self::Output::into)
    }

    fn short_circuit_combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &T,
    ) -> Result<Value, RuntimeError>
    where
        Self: ShortCircuitingOperator,
    {
        if self.short_circuit(&lhs) {
            Ok(lhs)
        } else {
            rhs.host(visitor)
        }
    }
}

pub trait ShortCircuitingOperator {
    fn short_circuit(&self, lhs: &Value) -> bool;
}

impl BinaryOperator<LogicalAnd> for OrOperator {
    type Output = Value;
    type Error = Infallible;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<Self::Output, Self::Error> {
        Ok(if lhs.is_truthy() { lhs } else { rhs })
    }
}

impl ExpressionVisitCombiner<Interpreter, LogicalAnd> for OrOperator {
    type Output = Value;
    type Error = RuntimeError;

    fn combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &LogicalAnd,
    ) -> Result<Self::Output, Self::Error> {
        self.short_circuit_combine(lhs, visitor, rhs)
    }
}

impl ShortCircuitingOperator for OrOperator {
    fn short_circuit(&self, lhs: &Value) -> bool {
        lhs.is_truthy()
    }
}

impl BinaryOperator<Equality> for AndOperator {
    type Output = Value;
    type Error = Infallible;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<Self::Output, Self::Error> {
        Ok(if lhs.is_truthy() { rhs } else { lhs })
    }
}

impl ExpressionVisitCombiner<Interpreter, Equality> for AndOperator {
    type Output = Value;
    type Error = RuntimeError;

    fn combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &Equality,
    ) -> Result<Self::Output, Self::Error> {
        self.short_circuit_combine(lhs, visitor, rhs)
    }
}

impl ShortCircuitingOperator for AndOperator {
    fn short_circuit(&self, lhs: &Value) -> bool {
        !lhs.is_truthy()
    }
}

impl ExpressionVisitCombiner<Interpreter, Comparison> for EqualityOperator {
    type Output = Value;
    type Error = RuntimeError;

    fn combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &Comparison,
    ) -> Result<Self::Output, Self::Error> {
        self.eager_combine(lhs, visitor, rhs)
    }
}

impl BinaryOperator<Comparison> for EqualityOperator {
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

impl ExpressionVisitCombiner<Interpreter, Sum> for ComparisonOperator {
    type Output = Value;
    type Error = RuntimeError;

    fn combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &Sum,
    ) -> Result<Self::Output, Self::Error> {
        self.eager_combine(lhs, visitor, rhs)
    }
}

impl BinaryOperator<Sum> for ComparisonOperator {
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

impl ExpressionVisitCombiner<Interpreter, Factor> for SumOperator {
    type Output = Value;
    type Error = RuntimeError;

    fn combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &Factor,
    ) -> Result<Self::Output, Self::Error> {
        self.eager_combine(lhs, visitor, rhs)
    }
}

impl BinaryOperator<Factor> for SumOperator {
    type Output = Value;
    type Error = TypeError;

    fn evaluate(self, lhs: Value, rhs: Value) -> Result<Self::Output, Self::Error> {
        if self == Self::Plus {
            match (&lhs, &rhs) {
                (Value::String(lhs), Value::String(rhs)) => {
                    let string = lhs.as_ref().to_owned() + rhs.as_ref();
                    return Ok(Value::String(string.into()));
                }
                (Value::String(_), _) => return Err(TypeError::SumString(rhs)),
                (_, Value::String(_)) => return Err(TypeError::SumString(lhs)),
                _ => (),
            };
        }
        let Some(lhs) = lhs.float() else {
            return Err(TypeError::Sum(self, lhs));
        };
        let Some(rhs) = rhs.float() else {
            return Err(TypeError::Sum(self, rhs));
        };
        let output = match self {
            Self::Minus => lhs - rhs,
            Self::Plus => lhs + rhs,
        };
        Ok(Value::Number(output))
    }
}

impl ExpressionVisitCombiner<Interpreter, Unary> for FactorOperator {
    type Output = Value;
    type Error = RuntimeError;

    fn combine(
        &self,
        lhs: Value,
        visitor: &mut Interpreter,
        rhs: &Unary,
    ) -> Result<Self::Output, Self::Error> {
        self.eager_combine(lhs, visitor, rhs)
    }
}

impl BinaryOperator<Unary> for FactorOperator {
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

impl ComparisonOperator {
    pub fn cast(self, value: Value) -> Result<f64, TypeError> {
        value.float().ok_or(TypeError::Comparison(self, value))
    }
}

impl FactorOperator {
    pub fn cast(self, lhs: Value) -> Result<f64, TypeError> {
        lhs.float().ok_or(TypeError::Factor(self, lhs))
    }
}

impl UnaryOperator {
    pub(crate) fn evaluate(self, value: Value) -> Result<Value, TypeError> {
        match self {
            Self::Minus => {
                let value = value.float().ok_or(TypeError::UnaryMinus(value))?;
                Ok(Value::Number(-value))
            }
            Self::Not => Ok(Value::Boolean(!value.is_truthy())),
        }
    }
}
