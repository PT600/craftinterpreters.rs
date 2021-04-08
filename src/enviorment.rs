use std::{cell::RefCell, collections::HashMap, ffi::FromBytesWithNulError, rc::Rc};

use anyhow::{bail, Context, Result};
use smol_str::SmolStr;

use crate::value::Value;

#[derive(Clone, Default, Debug)]
pub struct Env {
    pub values: HashMap<SmolStr, Value>,
    pub returns: Option<Value>,
    pub enclosing: Option<Rc<RefCell<Env>>>,
    pub is_call: bool,
    pub level: u8,
}

// pub enum BlockState {
//     Returning,
//     Returned(Value),
//     Looping,
//     Breaking,
//     None,
// }

impl Env {
    pub fn new() -> Self {
        Self {
            is_call: false,
            ..Default::default()
        }
    }

    pub fn new_with_enclosing(enclosing: &Rc<RefCell<Env>>, is_call: bool) -> Self {
        Self {
            values: Default::default(),
            returns: None,
            enclosing: Some(enclosing.clone()),
            is_call,
            level: enclosing.borrow().level + 1,
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
                    enclosing
                        .borrow()
                        .get(name)
                        .map(|v| v.clone())
                        .context(format!("Undefined variable {}", name))
                } else {
                    bail!("Undefined variable {}", name)
                }
            }
        }
    }

    pub fn assign(&mut self, name: &SmolStr, v: Value) -> Result<u8> {
        if self.values.contains_key(name) {
            self.values.insert(name.clone(), v);
            Ok(self.level)
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().assign(name, v)
        } else {
            bail!("Undefined variable {:?}", name)
        }
    }

    pub fn returns(&mut self, value: Value) -> Result<()> {
        // if matches!(value, Value::Fun(_)) {
            // bail!("Return function is not support!")
        // }else 
        if self.is_call {
            assert!(self.returns.is_none(), "return is already set!");
            self.returns = Some(value);
            Ok(())
        } else {
            if let Some(enclosing) = &self.enclosing {
                enclosing.borrow_mut().returns(value)
            } else {
                bail!("Not in a call!");
            }
        }
    }

    pub fn is_global(&self) -> bool {
        self.enclosing.is_none()
    }

    pub fn is_returned(&self) -> bool {
        if self.is_call {
            self.returns.is_some()
        } else {
            if let Some(enclosing) = &self.enclosing {
                enclosing.borrow().is_returned()
            } else {
                false
            }
        }
    }

    pub fn returned(&self) -> Value {
        self.returns.clone().unwrap_or(Value::Nil)
    }

}
