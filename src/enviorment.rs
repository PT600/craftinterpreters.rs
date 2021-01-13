use std::collections::HashMap;

use crate::ast::Value;
use anyhow::{Context, Result, bail};
use smol_str::SmolStr;

#[derive(Clone)]
pub(crate) struct Env {
    pub values: HashMap<SmolStr, Value>,
    pub enclosing: Option<Box<Env>>,
}

impl Env {
    pub fn define(&mut self, name: SmolStr, v: Value) {
        self.values.insert(name, v);
    }

    pub fn get(&self, name: &SmolStr) -> Result<&Value> {
        match self.values.get(name) {
            Some(val) => Ok(val),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    enclosing.get(name).context(format!("Undefined variable {}", name))
                }else {
                    bail!("Undefined variable {}", name)
                }
            }
        }
    }

    pub fn assign(&mut self, name: &SmolStr, v: Value) -> Result<()> {
        if self.values.contains_key(name) {
            self.values.insert(name.clone(), v);
            Ok(())
        }else if let Some(enclosing) = &mut self.enclosing {
            enclosing.assign(name, v)
        }else {
            bail!("Undefined variable {:?}", name)
        }
    }
}