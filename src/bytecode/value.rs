use std::{borrow::Borrow, cell::RefCell, fmt::Display, rc::Rc};

use anyhow::{bail, Result};

use crate::bytecode::debug;

use super::object::{ObjClosure, ObjFunction, ObjString, Object};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    ObjString(*const ObjString),
    ObjFunction(Rc<ObjFunction>),
    ObjClosure(Rc<RefCell<ObjClosure>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::ObjString(s) => writeln!(f, "ObjString: {:?}", unsafe { &**s }),
            Value::ObjFunction(fun) => debug::fmt_fun(fun, f),
            Value::ObjClosure(closure) => debug::fmt_closure(closure.clone(), f),
            _ => writeln!(f, "{:?}", self),
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
    pub fn as_str(&self) -> Result<String> {
        match self {
            Value::ObjString(obj) => Ok((&unsafe { &**obj }.data).into()),
            Value::Number(num) => Ok(format!("{}", num)),
            Value::Boolean(bool) => Ok(format!("{}", bool)),
            Value::Nil => Ok("Nil".into()),
            Value::ObjFunction(fun) => Ok(format!("{}", fun)),
            Value::ObjClosure(closure) => Ok(format!("{:?}", closure)),
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
