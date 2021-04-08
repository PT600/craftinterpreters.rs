use crate::{ast::*, enviorment::Env};
use std::{
    cell::RefCell,
    fmt::{self, Display},
    rc::Rc,
};

use anyhow::{bail, Result};
use fmt::Debug;
use smol_str::SmolStr;

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Num(f64),
    Nil,
    String(String),
    NativeFun(Rc<NativeFun>),
    LoxFun(FunId, SmolStr),
    // Object(Object),
}

pub struct NativeFun {
    pub name: SmolStr,
    pub arity: usize,
    pub callable: fn(&[Value]) -> Result<Value>,
}

impl Debug for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFun({})", self.name)
    }
}

pub type FunId = u8;
pub struct LoxFun {
    pub id: FunId,
    pub closure: Rc<RefCell<Env>>,
    pub decl: FunDecl,
    pub ref_level: u8,
}
// prevent recursive loop
impl Debug for LoxFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fun: {:?}", self.decl)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Num(num) => write!(f, "{}", num),
            Value::Nil => write!(f, "nil"),
            Value::String(content) => write!(f, "{}", content),
            Value::NativeFun(fun) => write!(f, "Native({})", fun.name),
            Value::LoxFun(fun, name) => write!(f, "Native({})", name),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn is_fun(&self) -> bool {
        match self {
            Value::NativeFun(_) | Value::LoxFun(_, _) => true,
            _ => false,
        }
    }

    pub fn as_num(&self) -> Result<f64> {
        match self {
            Value::Num(num) => Ok(*num),
            _ => bail!("Operands must be a number"),
        }
    }

    pub fn is_equals(&self, rhs: &Value) -> bool {
        match (self, rhs) {
            (Value::Num(n1), Value::Num(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (_, _) => false,
        }
    }
}
