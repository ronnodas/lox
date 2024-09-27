mod environment;

use std::convert::Infallible;
use std::{fmt, io};

use itertools::Itertools as _;
use miette::Diagnostic;
use thiserror::Error;

use crate::parser::ast::{
    Atom, Binary, Grouping, Identifier, LValue, Node, Prefix, TypeError, Value, Visitor,
};
use crate::parser::{Parser, Span};
use crate::LoxError;

use environment::Environment;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl Visitor for Interpreter {
    type Output = (Value, Vec<String>);
    type Error = RuntimeError;

    fn visit_atom(&mut self, atom: &Atom, _span: Span) -> Result<Self::Output, Self::Error> {
        match atom {
            Atom::Value(value) => Ok((value.clone(), vec![])),
            Atom::Identifier(identifier) => self
                .environment
                .get(identifier)
                .ok_or(RuntimeError::UndeclaredVariable(Identifier::clone(
                    identifier,
                )))
                .map(|value| (value, vec![])),
        }
    }

    fn visit_prefix(
        &mut self,
        op: Prefix,
        arg: &Node,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        let (value, output) = arg.host(self)?;
        match op {
            Prefix::Minus => value
                .float()
                .map(|number| (Value::Number(-number), output))
                .ok_or_else(|| TypeError::UnaryMinus(value).into()),
            Prefix::Not => Ok((Value::Boolean(!value.is_truthy()), output)),
            Prefix::Print => {
                let mut output = output;
                output.push(format!("{value}"));
                Ok((Value::Nil, output))
            }
        }
    }

    fn visit_binary(
        &mut self,
        op: Binary,
        [left, right]: &[Node; 2],
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        match op {
            Binary::Logical(op) => {
                let (left, output_left) = left.host(self)?;
                if op.short_circuit(&left) {
                    Ok((left, output_left))
                } else {
                    let (right, output_right) = right.host(self)?;
                    let output = [output_left, output_right].concat();
                    Ok((right, output))
                }
            }
            Binary::Arithmetic(_) | Binary::Comparison(_) | Binary::Equality(_) => {
                let (left, output_left) = left.host(self)?;
                let (right, output_right) = right.host(self)?;
                let output = [output_left, output_right].concat();
                let value = op.evaluate(left, right)?;

                Ok((value, output))
            }
        }
    }

    fn visit_group(
        &mut self,
        grouping: Grouping,
        items: &[Node],
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        match grouping {
            Grouping::Brace => {
                self.environment.push();
                let (value, output) = items.iter().try_fold(
                    (Value::Nil, vec![]),
                    |(_, mut output), item| -> Result<Self::Output, Self::Error> {
                        let (value, new_output) = item.host(self)?;
                        output.extend(new_output);
                        Ok((value, output))
                    },
                )?;
                self.environment.pop();

                Ok((value, output))
            }
        }
    }

    fn visit_assignment(
        &mut self,
        left: &LValue,
        right: &Node,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        let (value, output) = right.host(self)?;
        self.environment
            .set(left, value.clone())
            .ok_or(RuntimeError::UndeclaredVariable(Identifier::clone(left)))?;
        Ok((value, output))
    }

    fn visit_declaration(
        &mut self,
        identifier: &Identifier,
        initializer: Option<&Node>,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        let (value, output) = initializer.map(|node| node.host(self)).transpose()?.unzip();
        self.environment.declare(identifier, value.clone());
        Ok((value.unwrap_or(Value::Nil), output.unwrap_or_default()))
    }

    fn visit_for(
        &mut self,
        condition: &Node,
        body: &Node,
        increment: Option<&Node>,
        _span: Span,
    ) -> Result<Self::Output, Self::Error> {
        let (mut value, mut prints) = (Value::Nil, vec![]);
        loop {
            let (condition, print) = condition.host(self)?;
            prints.extend(print);
            if !condition.is_truthy() {
                break;
            }
            let (body, print) = body.host(self)?;
            value = body;
            prints.extend(print);
            if let Some(increment) = increment {
                let (_, print) = increment.host(self)?;
                prints.extend(print);
            }
        }
        Ok((value, prints))
    }
}

impl Interpreter {
    pub fn interpret<'i>(
        &'i mut self,
        statements: impl IntoIterator<Item = Node> + 'i,
    ) -> Result<Vec<String>, RuntimeError> {
        statements
            .into_iter()
            .map(|declaration| declaration.host(self))
            .map_ok(|(_, output)| output)
            .flatten_ok()
            .collect()
    }

    pub fn run(&mut self, source: &str, print_last: bool) -> Result<Vec<String>, LoxError> {
        let mut statements: Vec<Node> = Parser::new(source)
            .try_collect()
            .map_err(LoxError::Parsing)?;
        if print_last {
            if let Some(node) = statements.pop() {
                let node = node.wrap_in_print();
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
