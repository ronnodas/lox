use std::collections::HashMap;
use std::mem;

use crate::parser::ast::Identifier;

use super::value::Value;

pub type Closure = HashMap<Identifier, Value>;

#[derive(Default, Clone, Debug, PartialEq)]
pub struct Environment {
    variables: Closure,
    enclosing: Option<Box<Self>>,
}

impl Environment {
    pub fn get(&self, identifier: &str) -> Option<&Value> {
        self.variables
            .get(identifier)
            .or_else(|| self.enclosing.as_ref()?.get(identifier))
    }

    #[must_use]
    pub fn set(&mut self, identifier: &Identifier, value: Value) -> Option<()> {
        match self.variables.get_mut(identifier) {
            Some(variable) => {
                *variable = value;
                Some(())
            }
            None => self.enclosing.as_mut()?.set(identifier, value),
        }
    }

    pub fn declare(&mut self, identifier: &str, value: Option<Value>) {
        _ = self
            .variables
            .insert(identifier.into(), value.unwrap_or_else(Value::nil));
    }

    pub fn block(&mut self) {
        let enclosing = mem::take(self);
        *self = Self {
            variables: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn end_block(&mut self) {
        if let Some(enclosing) = self.enclosing.take() {
            *self = *enclosing;
        }
    }

    pub fn global() -> Self {
        let variables = HashMap::from([("clock".into(), Value::built_in(Clock))]);
        Self {
            variables,
            enclosing: None,
        }
    }

    pub fn capture(&self) -> Closure {
        let mut closure = self
            .enclosing
            .as_ref()
            .map(|enclosing| enclosing.capture())
            .unwrap_or_default();
        closure.extend(self.variables.clone());
        closure
    }

    pub fn push(&mut self, closure: Closure) {
        self.enclosing = Some(Box::new(mem::take(self)));
        self.variables = closure;
    }

    pub fn pop(&mut self) -> Closure {
        let mut current = mem::take(&mut self.variables);
        *self = self.enclosing.take().map_or_else(Self::global, |x| *x);
        current.extend(self.capture());
        current
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Clock;
