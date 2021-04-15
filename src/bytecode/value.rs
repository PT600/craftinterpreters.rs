use anyhow::{Result, bail};

use super::object::{ObjString, Object};


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Nil,
  Number(f64),
  Boolean(bool),
  Object(*const Object),
  ObjString(*const ObjString),
}

impl Value {

  pub fn as_num(&self) -> Result<f64> {
    match self {
      Value::Number(num) => Ok(*num),
      _ => bail!("can't convert {:?} to number!", self)
    }
  }
  pub fn as_bool(&self) -> Result<bool> {
    match self {
      Value::Boolean(val) => Ok(*val),
      _ => bail!("can't convert {:?} to bool!", self)
    }
  }
  pub fn as_str(&self) -> Result<*const ObjString> {
    match self {
      Value::ObjString(obj) => Ok(*obj),
      _ => bail!("can't convert {:?} to str!", self)
    }
  }
}