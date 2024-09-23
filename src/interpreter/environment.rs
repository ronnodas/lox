use std::collections::HashMap;
use std::mem;

use crate::parser::ast::{Identifier, Value};

#[derive(Default)]
pub struct Environment {
    variables: HashMap<Identifier, Value>,
    enclosing: Option<Box<Self>>,
}

impl Environment {
    pub fn get(&self, identifier: &str) -> Option<Value> {
        self.variables
            .get(identifier)
            .cloned()
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
            .insert(identifier.into(), value.unwrap_or(Value::Nil));
    }

    pub fn push(&mut self) {
        let enclosing = mem::take(self);
        *self = Self {
            variables: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn pop(&mut self) {
        *self = *self.enclosing.take().unwrap_or_default();
    }
}
