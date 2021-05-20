use std::{fmt::Display, rc::Rc};

use anyhow::{bail, Result};

use super::object::{ObjFunction, ObjString, Object};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    ObjString(*const ObjString),
    ObjFunction(ObjFunction),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::ObjString(s) => {
              write!(f, "{:?}", unsafe { &**s })
            },
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Value {
    pub fn as_num(&self) -> Result<f64> {
        match self {
            Value::Number(num) => Ok(*num),
            _ => bail!("can't convert {:?} to number!", self),
        }
    }
    pub fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Boolean(val) => Ok(*val),
            _ => bail!("can't convert {:?} to bool!", self),
        }
    }
    pub fn as_str(&self) -> Result<*const ObjString> {
        match self {
            Value::ObjString(obj) => Ok(*obj),
            _ => bail!("can't convert {:?} to str!", self),
        }
    }
    pub fn is_false(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Boolean(false) => true,
            _ => false,
        }
    }
}
