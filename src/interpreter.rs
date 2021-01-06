use crate::ast::{Expr::*, TokenType::*, *};
use anyhow::{bail, Result};

pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate(&self, expr: &Expr) -> Result<Value> {
        let value = match expr {
            Literal(kind) => match kind {
                LiteralKind::Num(num) => Value::Num(*num),
                LiteralKind::Boolean(v) => Value::Boolean(*v),
                LiteralKind::Nil => Value::Nil,
                LiteralKind::String(content) => Value::String(content.clone()),
                _ => bail!("todo for literalKind: {:?}!", kind),
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
            }
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
                    PLUS => match (left, right) {
                        (Value::Num(left), Value::Num(right)) => Value::Num(left + right),
                        (Value::String(left), Value::String(right)) => Value::String(left + &right),
                        _ => bail!("Operands must be two numbers or strings!"),
                    }
                    SPLASH => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left / right)
                    }
                    STAR => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left * right)
                    }
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
        };
        Ok(value)
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Num(f64),
    Nil,
    String(String),
    // Object(Object),
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }

    fn as_num(&self) -> Result<f64> {
        match self {
            Value::Num(num) => Ok(*num),
            _ => bail!("Operands must be a number"),
        }
    }

    fn is_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, _) | (_, Value::Nil) => false,
            _ => self == other,
        }
    }
}
