use anyhow::{Result, bail};
#[derive(Debug, Clone)]
pub enum Value {
  Nil,
  Number(f64),
  Boolean(bool),
  Str(String),
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
  pub fn as_str(&self) -> Result<String> {
    match self {
      Value::Str(str) => Ok(str.clone()),
      _ => bail!("can't convert {:?} to str!", self)
    }
  }
}