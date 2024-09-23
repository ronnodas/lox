use std::collections::HashMap;

use crate::parser::ast::{Identifier, Value};

#[derive(Default)]
pub struct Environment {
    variables: HashMap<Identifier, Value>,
}

impl Environment {
    pub fn get(&self, identifier: &str) -> Option<Value> {
        self.variables.get(identifier).cloned()
    }

    #[must_use]
    pub fn set(&mut self, identifier: &Identifier, value: Value) -> Option<()> {
        self.variables.get_mut(identifier).map(|v| *v = value)
    }

    pub fn declare(&mut self, identifier: &str, value: Option<Value>) {
        _ = self
            .variables
            .insert(identifier.into(), value.unwrap_or(Value::Nil));
    }
}
