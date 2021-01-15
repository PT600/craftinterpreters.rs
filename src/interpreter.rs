use std::{collections::HashMap, mem};

use crate::{
    ast::{Expr::*, TokenType::*, *},
    ast_printer::print,
    enviorment::Env,
};
use anyhow::{bail, Result};
use smol_str::SmolStr;

pub struct Interpreter {
    env: Env,
    loop_breakings: Vec<bool>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let env = Env {
            values: HashMap::new(),
            enclosing: None,
        };
        let loop_breakings = vec![];
        Self {
            env,
            loop_breakings,
        }
    }
    pub fn interpret(&mut self, stmts: Vec<Result<Stmt>>) -> Result<()> {
        for stmt in &stmts {
            match stmt {
                Ok(stmt) => {
                    let result = self.eval_stmt(stmt);
                    result.unwrap_or_else(|err| println!("err: {:?}", err))
                }
                Err(err) => {
                    println!("err: {:?}", err);
                }
            }
        }
        Ok(())
    }

    fn is_breaking(&self) -> bool{
        *self.loop_breakings.last().unwrap_or(&false)
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        if self.is_breaking() {
            return Ok(());
        }
        match stmt {
            Stmt::ExprStmt(expr) => {
                let value = self.eval_expr(expr)?;
                println!("{}", value)
            }
            Stmt::PrintStmt(expr) => {
                let value = self.eval_expr(expr)?;
                println!("{}", value);
            }
            Stmt::VarDecl(var, expr) => {
                let v = if let Some(expr) = expr {
                    self.eval_expr(expr)?
                } else {
                    Value::Nil
                };
                self.env.define(var.clone(), v)
            }
            Stmt::BlockStmt(block) => {
                let env = mem::replace(
                    &mut self.env,
                    Env {
                        values: HashMap::new(),
                        enclosing: None,
                    },
                );
                self.env.enclosing = Some(Box::new(env));
                for stmt in block {
                    self.eval_stmt(stmt)?;
                }
                let env = *self.env.enclosing.take().expect("Should have enclosing!");
                self.env = env;
            }
            Stmt::IF(if_stmt) => {
                let cond = self.eval_expr(&if_stmt.cond)?;
                if cond.is_truthy() {
                    self.eval_stmt(&if_stmt.then)?;
                } else if let Some(els) = &if_stmt.els {
                    self.eval_stmt(els)?;
                }
            }
            Stmt::While(while_stmt) => {
                self.loop_breakings.push(false);
                while self.eval_expr(&while_stmt.cond)?.is_truthy() {
                    self.eval_stmt(&while_stmt.body)?;
                    if self.is_breaking() {
                        break;
                    }
                }
                self.loop_breakings.pop();
            }
            Stmt::Break => {
                if let Some(breaking_loop) = self.loop_breakings.last_mut() {
                    *breaking_loop = true;
                } else {
                    return bail!("break should be in loop");
                }
            }
        }
        Ok(())
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        let value = match expr {
            Literal(kind) => match kind {
                LiteralKind::Num(num) => Value::Num(*num),
                LiteralKind::Boolean(v) => Value::Boolean(*v),
                LiteralKind::Nil => Value::Nil,
                LiteralKind::String(content) => Value::String(content.clone()),
            },
            Variable(var) => {
                let v = self.env.get(var)?;
                v.clone()
            }
            Unary(expr) => match &expr.operator.ttype {
                TokenType::BANG => {
                    let val = self.eval_expr(&expr.right)?;
                    Value::Boolean(!val.is_truthy())
                }
                TokenType::MINUS => {
                    let val = self.eval_expr(&expr.right)?;
                    let val = val.as_num()?;
                    Value::Num(-val)
                }
                _ => bail!("Unkown unary operator: {:?}", expr.operator),
            },
            Binary(expr) => {
                let left = self.eval_expr(&expr.left)?;
                let right = self.eval_expr(&expr.right)?;
                match &expr.operator.ttype {
                    BangEqual => Value::Boolean(!left.is_equal(&right)),
                    EqualEqual => Value::Boolean(left.is_equal(&right)),
                    GREATER => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Boolean(left > right)
                    }
                    GreaterEqual => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Boolean(left >= right)
                    }
                    LESS => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Boolean(left < right)
                    }
                    LessEqual => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Boolean(left <= right)
                    }
                    MINUS => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left - right)
                    }
                    MinusEqual => self.ops_assign(expr, MINUS)?,
                    PLUS => match (&left, &right) {
                        (Value::Num(left), Value::Num(right)) => Value::Num(left + right),
                        _ => Value::String(format!("{}{}", left, right)),
                    },
                    PlusEqual => self.ops_assign(expr, PLUS)?,
                    SLASH => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left / right)
                    }
                    SlashEqual => self.ops_assign(expr, SLASH)?,
                    STAR => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left * right)
                    }
                    StarEqual => self.ops_assign(expr, STAR)?,
                    _ => bail!("Unkown binary operator: {:?}", expr.operator),
                }
            }
            Grouping(expr) => self.eval_expr(expr)?,
            Ternary(expr) => {
                let cond = self.eval_expr(&expr.cond)?;
                match cond {
                    Value::Boolean(cond) => {
                        if cond {
                            self.eval_expr(&expr.left)?
                        } else {
                            self.eval_expr(&expr.right)?
                        }
                    }
                    _ => bail!("Operands must be a condition!"),
                }
            }
            Assign(var, expr) => {
                let value = self.eval_expr(&*expr)?;
                self.env.assign(var, value.clone())?;
                value
            }
            Logic(expr) => {
                let left = self.eval_expr(&expr.left)?;
                match (expr.is_and, left.is_truthy()) {
                    (true, false) | (false, true) => left,
                    _ => self.eval_expr(&expr.right)?,
                }
            }
        };
        Ok(value)
    }

    fn ops_assign(&mut self, expr: &Box<BinaryExpr>, op: TokenType) -> Result<Value> {
        let left = &expr.left;
        let mut bin_expr = *expr.clone();
        bin_expr.operator.ttype = op;
        let expr = Expr::Binary(Box::new(bin_expr));
        let result = self.eval_expr(&expr)?;
        match &left {
            Variable(var) => {
                self.env.assign(var, result.clone())?;
            }
            _ => bail!("{:?} is not a Variable", left),
        }
        Ok(result)
    }
    pub fn env(&self, name: &SmolStr) -> Result<&Value> {
        self.env.get(name)
    }
}

#[cfg(test)]
#[path = "./interpreter_test.rs"]
mod interpreter_test;
