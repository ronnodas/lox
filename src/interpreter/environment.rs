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

    pub fn set(&mut self, identifier: &str, value: Value) {
        _ = self.variables.insert(identifier.into(), value);
    }
}
