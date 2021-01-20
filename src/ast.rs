use std::{cell::RefCell, fmt::{self, Display}, rc::Rc};

use crate::{enviorment::Env, scanner::*};
use anyhow::{Result, bail};
use fmt::Debug;
use smol_str::SmolStr;

use crate::interpreter::Interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(LiteralKind),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Grouping(Box<Expr>),
    Ternary(Box<TernaryExpr>),
    Variable(SmolStr),
    Assign(SmolStr, Box<Expr>),
    Logic(Box<LogicExpr>),
    Call(Box<CallExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Expr,
    pub paren: Token,
    pub arguments: Vec<Expr>
}
#[derive(Debug, Clone, PartialEq)]
pub struct LogicExpr {
    pub is_and: bool,
    pub left: Expr,
    pub right: Expr
}
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Boolean(bool),
    Nil,
    String(String),
    Num(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TernaryExpr {
    pub cond: Expr,
    pub left: Expr,
    pub right: Expr,
}

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

pub struct LoxFun {
    pub closure: Rc<RefCell<Env>>,
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


#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
    ReturnStmt(Option<Expr>),
    VarDecl(SmolStr, Option<Expr>),
    BlockStmt(Vec<Stmt>),
    IF(Box<IfStmt>),
    While(Box<WhileStmt>),
    Break,
    FunDecl(FunDecl)
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunDecl {
    pub name: SmolStr,
    pub params: Vec<SmolStr>,
    pub body: Box<Stmt>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Stmt,
    pub els: Option<Stmt>
}
#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Stmt,
}
