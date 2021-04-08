use std::{cell::RefCell, fmt::{self, Display}, rc::{Rc, Weak}};

use crate::{enviorment::Env, scanner::*};
use anyhow::{Result, bail};
use fmt::Debug;
use smol_str::SmolStr;
use slotmap::SlotMap;

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
    pub body: Rc<Stmt>,
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
