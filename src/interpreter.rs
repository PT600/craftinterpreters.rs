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
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let env = Env {
            values: HashMap::new(),
            enclosing: None,
        };
        Self { env }
    }
    pub fn interpret(&mut self, stmts: Vec<Result<Stmt>>) -> Result<()> {
        for stmt in &stmts {
            match stmt {
                Ok(stmt) => {
                    let result = self.eval(stmt);
                    result.unwrap_or_else(|err| println!("err: {:?}", err))
                }
                Err(err) => {
                    println!("err: {:?}", err);
                }
            }
        }
        Ok(())
    }

    pub fn eval(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::ExprStmt(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value)
            }
            Stmt::PrintStmt(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value);
            }
            Stmt::VarDecl(var, expr) => {
                let v = if let Some(expr) = expr {
                    self.evaluate(expr)?
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
                    self.eval(stmt)?
                }
                let env = *self.env.enclosing.take().expect("Should have enclosing!");
                self.env = env;
            }
        }
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        let value = match expr {
            Literal(kind) => match kind {
                LiteralKind::Num(num) => Value::Num(*num),
                LiteralKind::Boolean(v) => Value::Boolean(*v),
                LiteralKind::Nil => Value::Nil,
                LiteralKind::String(content) => Value::String(content.clone()),
                _ => bail!("todo for literalKind: {:?}!", kind),
            },
            Variable(var) => {
                let v = self.env.get(var)?;
                v.clone()
            }
            Unary(expr) => match &expr.operator.ttype {
                TokenType::BANG => {
                    let val = self.evaluate(&expr.right)?;
                    Value::Boolean(!val.is_truthy())
                }
                TokenType::MINUS => {
                    let val = self.evaluate(&expr.right)?;
                    let val = val.as_num()?;
                    Value::Num(-val)
                }
                _ => bail!("Unkown unary operator: {:?}", expr.operator),
            },
            Binary(expr) => {
                let left = self.evaluate(&expr.left)?;
                let right = self.evaluate(&expr.right)?;
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
                    PLUS => match (left, right) {
                        (Value::Num(left), Value::Num(right)) => Value::Num(left + right),
                        (Value::String(left), Value::String(right)) => Value::String(left + &right),
                        _ => bail!("Operands must be two numbers or strings!"),
                    },
                    PlusEqual => self.ops_assign(expr, PLUS)?,
                    SLASH => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left / right)
                    }
                    SlashEqual => {
                        self.ops_assign(expr, SLASH)?
                    }
                    STAR => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left * right)
                    }
                    StarEqual => self.ops_assign(expr, STAR)?,
                    _ => bail!("Unkown binary operator: {:?}", expr.operator),
                }
            }
            Grouping(expr) => self.evaluate(expr)?,
            Ternary(expr) => {
                let cond = self.evaluate(&expr.cond)?;
                match cond {
                    Value::Boolean(cond) => {
                        if cond {
                            self.evaluate(&expr.left)?
                        } else {
                            self.evaluate(&expr.right)?
                        }
                    }
                    _ => bail!("Operands must be a condition!"),
                }
            }
            Assign(var, expr) => {
                let value = self.evaluate(&*expr)?;
                self.env.assign(var, value.clone())?;
                value
            }
        };
        Ok(value)
    }

    fn ops_assign(&mut self, expr: &Box<BinaryExpr>, op: TokenType) -> Result<Value> {
        let left = &expr.left;
        let mut bin_expr = *expr.clone();
        bin_expr.operator.ttype = op;
        let expr = Expr::Binary(Box::new(bin_expr));
        let result = self.evaluate(&expr)?;
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
