mod environment;

use std::convert::Infallible;
use std::{fmt, io};

use itertools::Itertools as _;
use miette::Diagnostic;
use thiserror::Error;

use crate::parser::ast::{
    Atom, Binary, ExpressionNode, ExpressionVisitor, Identifier, LValue, Prefix, StatementNode,
    StatementVisitor, TypeError, Value,
};
use crate::parser::{Parser, Span};
use crate::LoxError;

use environment::Environment;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl StatementVisitor for Interpreter {
    type Output = Vec<String>;
    type Error = RuntimeError;

    fn visit_expression(
        &mut self,
        expression: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error> {
        _ = expression.host(self)?;
        Ok(vec![])
    }

    fn visit_print(&mut self, expression: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        let value = expression.host(self)?;
        Ok(vec![value.to_string()])
    }

    fn visit_block(&mut self, items: &[StatementNode]) -> Result<Self::Output, Self::Error> {
        self.environment.push();
        let output = items.iter().try_fold(
            vec![],
            |mut output, item| -> Result<Self::Output, Self::Error> {
                let new_output = item.host(self)?;
                output.extend(new_output);
                Ok(output)
            },
        )?;
        self.environment.pop();

        Ok(output)
    }

    fn visit_declaration(
        &mut self,
        identifier: &Identifier,
        initializer: Option<&ExpressionNode>,
    ) -> Result<Self::Output, Self::Error> {
        let value = initializer.map(|node| node.host(self)).transpose()?;
        self.environment.declare(identifier, value);
        Ok(vec![])
    }

    fn visit_while(
        &mut self,
        condition: &ExpressionNode,
        body: &StatementNode,
    ) -> Result<Self::Output, Self::Error> {
        let mut prints = vec![];
        loop {
            let condition = condition.host(self)?;
            if !condition.is_truthy() {
                break;
            }
            let print = body.host(self)?;
            prints.extend(print);
        }
        Ok(prints)
    }
}

impl ExpressionVisitor for Interpreter {
    type Output = Value;
    type Error = RuntimeError;

    fn visit_atom(&mut self, atom: &Atom, _span: Span) -> Result<Self::Output, Self::Error> {
        match atom {
            Atom::Value(value) => Ok(value.clone()),
            Atom::Identifier(identifier) => {
                self.environment
                    .get(identifier)
                    .ok_or(RuntimeError::UndeclaredVariable(Identifier::clone(
                        identifier,
                    )))
            }
        }
    }

    fn visit_prefix(
        &mut self,
        op: Prefix,
        arg: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error> {
        let value = arg.host(self)?;
        match op {
            Prefix::Minus => value
                .float()
                .map(|number| Value::Number(-number))
                .ok_or_else(|| TypeError::UnaryMinus(value).into()),
            Prefix::Not => Ok(Value::Boolean(!value.is_truthy())),
        }
    }

    fn visit_binary(
        &mut self,
        op: Binary,
        [left, right]: &[ExpressionNode; 2],
    ) -> Result<Self::Output, Self::Error> {
        match op {
            Binary::Logical(op) => {
                let left = left.host(self)?;
                if op.short_circuit(&left) {
                    Ok(left)
                } else {
                    right.host(self)
                }
            }
            Binary::Arithmetic(_) | Binary::Comparison(_) | Binary::Equality(_) => {
                let left = left.host(self)?;
                let right = right.host(self)?;
                op.evaluate(left, right).map_err(RuntimeError::from)
            }
        }
    }

    fn visit_assignment(
        &mut self,
        left: &LValue,
        right: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error> {
        let value = right.host(self)?;
        self.environment
            .set(left, value.clone())
            .ok_or(RuntimeError::UndeclaredVariable(Identifier::clone(left)))?;
        Ok(value)
    }
}

impl Interpreter {
    pub fn interpret<'i>(
        &'i mut self,
        statements: impl IntoIterator<Item = StatementNode> + 'i,
    ) -> Result<Vec<String>, RuntimeError> {
        statements
            .into_iter()
            .map(|statement| statement.host(self))
            .flatten_ok()
            .collect()
    }

    pub fn run(&mut self, source: &str, print_last: bool) -> Result<Vec<String>, LoxError> {
        let mut statements: Vec<StatementNode> = Parser::new(source)
            .try_collect()
            .map_err(LoxError::Parsing)?;
        if print_last {
            if let Some(node) = statements.pop() {
                let node = node.implicit_print();
                statements.push(node);
            }
        }

        Ok(self.interpret(statements)?)
    }

    pub fn run_and_print(&mut self, line: &str) {
        match self.run(line, false) {
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
        // TODO use something like https://crates.io/crates/rustyline
        // TODO QoL:
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

            match self.run(&line, true) {
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
