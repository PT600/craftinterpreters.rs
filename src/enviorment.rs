use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::Value;
use anyhow::{Context, Result, bail};
use smol_str::SmolStr;

#[derive(Clone, Default, Debug)]
pub struct Env {
    pub values: HashMap<SmolStr, Value>,
    pub returns: Option<Value>,
    pub enclosing: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with_enclosing(enclosing: &Rc<RefCell<Env>>) -> Self {
        Self {
            values: Default::default(),
            returns: None,
            enclosing: Some(enclosing.clone()),
        }
    }

    pub fn define(&mut self, name: SmolStr, v: Value) {
        self.values.insert(name, v);
    }

    pub fn get(&self, name: &SmolStr) -> Result<Value> {
        match self.values.get(name) {
            Some(val) => Ok(val.clone()),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    enclosing.borrow().get(name).map(|v|v.clone()).context(format!("Undefined variable {}", name))
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
            enclosing.borrow_mut().assign(name, v)
        }else {
            bail!("Undefined variable {:?}", name)
        }
    }

    pub fn returns(&mut self, value: Value) {
        assert!(self.returns.is_none(), "return is already set!");
        assert!(!self.is_global(), "can't return global!");
        self.returns = Some(value);
    }

    pub fn is_global(&self) -> bool{
        self.enclosing.is_none()
    }

    pub fn is_returned(&self) -> bool {
        self.returns.is_some()
    }

    pub fn returned(&self) -> Value {
        self.returns.clone().unwrap_or(Value::Nil)
    }
}