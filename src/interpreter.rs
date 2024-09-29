pub mod environment;
pub mod value;

use std::convert::Infallible;
use std::{io, mem, time};

use itertools::Itertools as _;
use miette::Diagnostic;
use thiserror::Error;
use value::{Function, TypeError, Value};

use crate::parser::ast::{
    Atom, Binary, ExpressionNode, ExpressionVisitor, Identifier, LValue, Prefix, StatementNode,
    StatementVisitor,
};
use crate::parser::{Parser, Span};
use crate::LoxError;

pub use environment::Clock;
use environment::Environment;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
    prints: Vec<String>,
}

impl StatementVisitor for Interpreter {
    type Output = ();
    type Error = Control;

    fn visit_expression(
        &mut self,
        expression: &ExpressionNode,
    ) -> Result<Self::Output, Self::Error> {
        _ = expression.host(self)?;
        Ok(())
    }

    fn visit_print(&mut self, expression: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        let value = expression.host(self)?;
        let string = value
            .as_string()
            .map_or_else(|| value.to_string(), str::to_owned);
        self.prints.push(string);
        Ok(())
    }

    fn visit_block(&mut self, items: &[StatementNode]) -> Result<Self::Output, Self::Error> {
        self.environment.block();
        let output = items.iter().try_for_each(|item| item.host(self));
        self.environment.end_block();
        output
    }

    fn visit_declaration(
        &mut self,
        identifier: &Identifier,
        initializer: Option<&ExpressionNode>,
    ) -> Result<Self::Output, Self::Error> {
        let value = initializer.map(|node| node.host(self)).transpose()?;
        self.environment.declare(identifier, value);
        Ok(())
    }

    fn visit_if(
        &mut self,
        condition: &ExpressionNode,
        then_body: &StatementNode,
        else_body: Option<&StatementNode>,
    ) -> Result<Self::Output, Self::Error> {
        let condition = condition.host(self)?;
        if condition.is_truthy() {
            then_body.host(self)
        } else if let Some(else_body) = else_body {
            else_body.host(self)
        } else {
            Ok(())
        }
    }

    fn visit_while(
        &mut self,
        condition: &ExpressionNode,
        body: &StatementNode,
    ) -> Result<Self::Output, Self::Error> {
        loop {
            let condition = condition.host(self)?;
            if !condition.is_truthy() {
                break;
            }
            body.host(self)?;
        }
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        name: &Identifier,
        args: &[Identifier],
        body: &StatementNode,
    ) -> Result<Self::Output, Self::Error> {
        let args = args.to_vec();
        let body = Box::new(body.clone());
        let function = Function {
            name: Identifier::clone(name),
            args,
            body,
            // TODO only capture variables appearing in body
            closure: self.environment.capture().into(),
        };
        self.environment
            .declare(name, Some(Value::function(function)));
        Ok(())
    }

    fn visit_return(&mut self, expression: &ExpressionNode) -> Result<Self::Output, Self::Error> {
        let value = expression.host(self)?;
        Err(Control::Return(value))
    }
}

impl ExpressionVisitor for Interpreter {
    type Output = Value;
    type Error = RuntimeError;

    fn visit_atom(&mut self, atom: &Atom, _span: Span) -> Result<Self::Output, Self::Error> {
        match atom {
            Atom::Literal(value) => Ok(value.into()),
            Atom::Identifier(identifier) => self
                .environment
                .get(identifier)
                .ok_or(RuntimeError::UndeclaredVariable(Identifier::clone(
                    identifier,
                )))
                .cloned(),
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
                .map(|number| Value::number(-number))
                .ok_or_else(|| TypeError::UnaryMinus(value.to_string()).into()),
            Prefix::Not => Ok(Value::boolean(!value.is_truthy())),
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
            Binary::Field => todo!(),
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

    fn visit_call(
        &mut self,
        callee: &ExpressionNode,
        args: &[ExpressionNode],
    ) -> Result<Self::Output, Self::Error> {
        let callee = callee.host(self)?;
        let callee = callee.callable()?;
        let args: Vec<Value> = args.iter().map(|arg| arg.host(self)).try_collect()?;
        if callee.arity() == args.len() {
            callee.call(self, args)
        } else {
            Err(RuntimeError::ArityMismatch(callee.arity(), args.len()))
        }
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
            .try_for_each(|e| e)
            .map_err(Control::outside)?;
        Ok(mem::take(&mut self.prints))
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
            environment: Environment::global(),
            prints: Vec::new(),
        }
    }

    fn function_call(
        &mut self,
        function: &Function,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.environment.push(function.closure.take());
        for (name, value) in function.args.iter().zip(args) {
            self.environment.declare(name, Some(value));
        }
        let output = match function.body.host(self) {
            Ok(()) => Ok(Value::nil()),
            Err(Control::Return(value)) => Ok(value),
            Err(Control::Error(e)) => Err(e),
        };
        _ = function.closure.replace(self.environment.pop());
        output
    }
}

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
    fn arity(&self) -> usize;
}

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::number(
            time::SystemTime::now()
                .duration_since(time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs_f64(),
        ))
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        interpreter.function_call(self, args)
    }

    fn arity(&self) -> usize {
        self.args.len()
    }
}

pub enum Control {
    Error(RuntimeError),
    Return(Value),
}

impl Control {
    fn outside(self) -> RuntimeError {
        match self {
            Self::Error(v) => v,
            Self::Return(_) => RuntimeError::ReturnOutsideFunction,
        }
    }
}

impl From<RuntimeError> for Control {
    fn from(v: RuntimeError) -> Self {
        Self::Error(v)
    }
}

#[derive(Debug, Diagnostic, Error, PartialEq, Eq)]
pub enum RuntimeError {
    #[error(transparent)]
    Type(TypeError),
    #[error("{0} is not defined")]
    UndeclaredVariable(Identifier),
    #[error("Expected {0} arguments, got {1}")]
    ArityMismatch(usize, usize),
    #[error("Used return outside function")]
    ReturnOutsideFunction,
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
