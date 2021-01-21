use crate::{ast::*, enviorment::Env};
use std::{cell::RefCell, fmt::{self, Display}, rc::{Rc, Weak}};

use anyhow::{Result, bail};
use fmt::Debug;
use smol_str::SmolStr;
use slotmap::{new_key_type};

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Num(f64),
    Nil,
    String(String),
    Fun(Rc<FunKind>),
    // Object(Object),
}
#[derive(Debug)]
pub enum FunKind {
    Native(NativeFun),
    Lox(LoxFun)
}

use FunKind::*;
impl FunKind {
    pub fn arity(&self) -> usize {
        match self {
            Native(fun) => fun.arity as usize,
            Lox(fun) => fun.fun.params.len(),
        }
    }

}
impl Display for FunKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Native(fun) => write!(f, "Native({})", fun.name),
            Lox(fun) => write!(f, "LoxFun({})", fun.fun.name)
        }
    }
}

pub struct NativeFun {
    pub name: SmolStr,
    pub arity: u8,
    pub callable: fn(&[Value]) -> Result<Value>,
}

impl Debug for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFun({})", self.name)
    }
}

new_key_type! {
    pub struct EnvKey;
}
pub struct LoxFun {
    pub closure: Weak<RefCell<Env>>,
    pub fun: FunDecl,
}
// prevent recursive loop
impl Debug for LoxFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fun: {:?}", self.fun)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Num(num) => write!(f, "{}",  num),
            Value::Nil => write!(f, "nil"),
            Value::String(content) => write!(f, "{}", content),
            Value::Fun(fun) => write!(f, "({})", fun),
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

