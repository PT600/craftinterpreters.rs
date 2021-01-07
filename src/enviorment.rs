use std::collections::HashMap;

use crate::ast::Value;
use anyhow::{Context, Result, bail};

pub(crate) struct Env {
    pub values: HashMap<String, Value>,
    pub enclosing: Option<Box<Env>>,
}

impl Env {
    pub fn define(&mut self, name: String, v: Value) {
        self.values.insert(name, v);
    }

    pub fn get(&self, name: &String) -> Result<&Value> {
        self.values.get(name).context(format!("Undefined variable {}", name))
    }

    pub fn assign(&mut self, name: &String, v: Value) -> Result<()> {
        if self.values.contains_key(name) {
            self.values.insert(name.clone(), v);
            Ok(())
        }else {
            bail!("Undefined variable {:?}", name)
        }
    }
}