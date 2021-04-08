use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::{Expr::*, *},
    enviorment::Env,
    scanner::{TokenType::*, *},
    value::*,
};
use anyhow::{bail, Context, Result};
use smol_str::SmolStr;

pub struct Interpreter {
    globals: Rc<RefCell<Env>>,
    loop_breakings: Vec<bool>,
    repl: bool,
    funs: HashMap<FunId, LoxFun>,
    max_fun_id: FunId,
}

impl Interpreter {
    pub fn new(repl: bool) -> Interpreter {
        let globals = Rc::new(RefCell::new(Env::new()));
        globals.borrow_mut().define(
            "clock".into(),
            Value::NativeFun(Rc::new(NativeFun {
                name: "clock".into(),
                arity: 0,
                callable: |_| {
                    let start = SystemTime::now();
                    let since_epoch = start.duration_since(UNIX_EPOCH)?;
                    Ok(Value::Num(since_epoch.as_millis() as f64))
                },
            })),
        );
        let loop_breakings = vec![];
        Self {
            globals,
            loop_breakings,
            repl,
            funs: Default::default(),
            max_fun_id: 0,
        }
    }

    fn next_fun_id(&mut self) -> FunId {
        self.max_fun_id += 1;
        self.max_fun_id
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
        self.execute_stmt(stmt, &self.globals.clone())
    }

    fn is_breaking(&self) -> bool {
        *self.loop_breakings.last().unwrap_or(&false)
    }

    pub fn execute_stmt(&mut self, stmt: &Stmt, env: &Rc<RefCell<Env>>) -> Result<()> {
        if self.is_breaking() || env.borrow().is_returned() {
            return Ok(());
        }
        match stmt {
            Stmt::ExprStmt(expr) => {
                let value = self.eval_expr(expr, env)?;
                if self.repl {
                    println!("{}", value)
                }
            }
            Stmt::PrintStmt(expr) => {
                let value = self.eval_expr(expr, env)?;
                println!("{}", value);
            }
            Stmt::ReturnStmt(expr) => {
                let result = if let Some(expr) = expr {
                    self.eval_expr(expr, env)?
                } else {
                    Value::Nil
                };
                env.borrow_mut().returns(result)?;
            }
            Stmt::VarDecl(var, expr) => {
                let v = if let Some(expr) = expr {
                    self.eval_expr(expr, env)?
                } else {
                    Value::Nil
                };
                env.borrow_mut().define(var.clone(), v)
            }
            Stmt::BlockStmt(block) => {
                let env = Rc::new(RefCell::new(Env::new_with_enclosing(env, false)));
                for stmt in block {
                    self.execute_stmt(stmt, &env)?;
                }
                println!("before env.strong_count: {}", Rc::strong_count(&env));
                self.expire_funs(env.borrow().level);
                println!("after env.strong_count: {}", Rc::strong_count(&env));
            }
            Stmt::IF(if_stmt) => {
                let cond = self.eval_expr(&if_stmt.cond, env)?;
                if cond.is_truthy() {
                    self.execute_stmt(&if_stmt.then, env)?;
                } else if let Some(els) = &if_stmt.els {
                    self.execute_stmt(els, env)?;
                }
            }
            Stmt::While(while_stmt) => {
                self.loop_breakings.push(false);
                while self.eval_expr(&while_stmt.cond, env)?.is_truthy() {
                    self.execute_stmt(&while_stmt.body, env)?;
                    if self.is_breaking() || env.borrow().is_returned() {
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
            Stmt::FunDecl(fun) => {
                let name = fun.name.clone();
                let fun = LoxFun {
                    id: self.next_fun_id(),
                    closure: Rc::clone(env),
                    decl: fun.clone(),
                    ref_level: env.borrow().level,
                };
                env.borrow_mut()
                    .define(name.clone(), Value::LoxFun(fun.id, name));
                self.funs.insert(fun.id, fun);
            }
        }
        Ok(())
    }

    pub fn eval_expr(&mut self, expr: &Expr, env: &Rc<RefCell<Env>>) -> Result<Value> {
        let value = match expr {
            Literal(kind) => match kind {
                LiteralKind::Num(num) => Value::Num(*num),
                LiteralKind::Boolean(v) => Value::Boolean(*v),
                LiteralKind::Nil => Value::Nil,
                LiteralKind::String(content) => Value::String(content.clone()),
            },
            Variable(var) => env.borrow().get(var)?,
            Unary(expr) => match &expr.operator.ttype {
                TokenType::BANG => {
                    let val = self.eval_expr(&expr.right, env)?;
                    Value::Boolean(!val.is_truthy())
                }
                TokenType::MINUS => {
                    let val = self.eval_expr(&expr.right, env)?;
                    let val = val.as_num()?;
                    Value::Num(-val)
                }
                _ => bail!("Unkown unary operator: {:?}", expr.operator),
            },
            Binary(expr) => {
                let left = self.eval_expr(&expr.left, env)?;
                let right = self.eval_expr(&expr.right, env)?;
                match &expr.operator.ttype {
                    BangEqual => Value::Boolean(!left.is_equals(&right)),
                    EqualEqual => Value::Boolean(left.is_equals(&right)),
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
                    MinusEqual => self.ops_assign(expr, MINUS, env)?,
                    PLUS => match (&left, &right) {
                        (Value::Num(left), Value::Num(right)) => Value::Num(left + right),
                        _ => Value::String(format!("{}{}", left, right)),
                    },
                    PlusEqual => self.ops_assign(expr, PLUS, env)?,
                    SLASH => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left / right)
                    }
                    SlashEqual => self.ops_assign(expr, SLASH, env)?,
                    STAR => {
                        let left = left.as_num()?;
                        let right = right.as_num()?;
                        Value::Num(left * right)
                    }
                    StarEqual => self.ops_assign(expr, STAR, env)?,
                    _ => bail!("Unkown binary operator: {:?}", expr.operator),
                }
            }
            Grouping(expr) => self.eval_expr(expr, env)?,
            Ternary(expr) => {
                let cond = self.eval_expr(&expr.cond, env)?;
                match cond {
                    Value::Boolean(cond) => {
                        if cond {
                            self.eval_expr(&expr.left, env)?
                        } else {
                            self.eval_expr(&expr.right, env)?
                        }
                    }
                    _ => bail!("Operands must be a condition!"),
                }
            }
            Assign(var, expr) => {
                let value = self.eval_expr(&*expr, env)?;
                let level = env.borrow_mut().assign(var, value.clone())?;
                if let Value::LoxFun(id, _) = value {
                    let fun = self
                        .funs
                        .get_mut(&id)
                        .context(format!("Can't find fun with id: {}", id))?;
                    if level < fun.ref_level {
                        fun.ref_level = level;
                    }
                }
                value
            }
            Logic(expr) => {
                let left = self.eval_expr(&expr.left, env)?;
                match (expr.is_and, left.is_truthy()) {
                    (true, false) | (false, true) => left,
                    _ => self.eval_expr(&expr.right, env)?,
                }
            }
            Call(call) => {
                let callee = self.eval_expr(&call.callee, env)?;
                let args: Result<Vec<Value>> = call
                    .arguments
                    .iter()
                    .map(|arg| self.eval_expr(arg, env))
                    .collect();
                let args = args?;
                return match callee {
                    Value::NativeFun(fun) => {
                        assert_eq!(args.len(), fun.arity, "args's len not match!");
                        (fun.callable)(&args)
                    }
                    Value::LoxFun(id, _) => {
                        let fun = self
                            .funs
                            .get(&id)
                            .context(format!("can't find fun with id: {}", id))?;
                        assert_eq!(args.len(), fun.decl.params.len(), "args's len not match!");
                        let env =
                            Rc::new(RefCell::new(Env::new_with_enclosing(&fun.closure, true)));
                        for (param, arg) in fun.decl.params.iter().zip(args.iter()) {
                            env.borrow_mut().define(param.clone(), arg.clone());
                        }
                        self.execute_stmt(&*fun.decl.body.clone(), &env)?;
                        let result = env.borrow().returned();
                        Ok(result)
                    },
                    _ => bail!("{} is not a fun!", callee),
                }
            }
        };
        Ok(value)
    }

    fn ops_assign(
        &mut self,
        expr: &Box<BinaryExpr>,
        op: TokenType,
        env: &Rc<RefCell<Env>>,
    ) -> Result<Value> {
        let left = &expr.left;
        let mut bin_expr = *expr.clone();
        bin_expr.operator.ttype = op;
        let expr = Expr::Binary(Box::new(bin_expr));
        let result = self.eval_expr(&expr, env)?;
        match &left {
            Variable(var) => {
                env.borrow_mut().assign(var, result.clone())?;
            }
            _ => bail!("{:?} is not a Variable", left),
        }
        Ok(result)
    }

    fn lookup(&self, name: &SmolStr) -> Result<Value> {
        self.globals.borrow().get(name)
    }

    pub fn expire_funs(&mut self, level: u8) {
        self.funs.retain(|_, f| f.ref_level < level);
    }
}

#[cfg(test)]
#[path = "./interpreter_test.rs"]
mod interpreter_test;
