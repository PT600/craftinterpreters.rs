use std::{collections::HashMap, rc::Rc, usize};

use anyhow::{bail, Context, Result};

use super::{
    compiler::compile,
    object::{ObjFunction, ObjString, Object},
    strings::Strings,
    table::Table,
    *,
};

use super::chunk::{
    Chunk,
    OpCode::{self, *},
};
use super::value::Value;

const STACK_MAX: usize = 256;

struct CallFrame<'a> {
    chunk: &'a Chunk,
    ip: usize,
    slots: usize,
}

impl<'a> CallFrame<'a> {
    fn read_u16(&mut self) -> u16 {
        let hight = (self.chunk.codes[self.ip] as u16) << 8;
        let low = self.chunk.codes[self.ip + 1] as u16;
        self.ip += 2;
        hight + low
    }

    fn read_const(&mut self) -> Value {
        assert!(self.ip < self.chunk.codes.len(), "array out of bound!");
        let idx = self.chunk.codes[self.ip];
        self.ip += 1;
        self.chunk.consts[idx as usize].clone()
    }

    fn read_string(&mut self) -> Result<*const ObjString> {
        if let Value::ObjString(id) = self.read_const() {
            return Ok(id);
        }
        bail!("xx")
    }

    fn read_byte(&mut self) -> Option<OpCode> {
        if self.ip < self.chunk.codes.len() {
            let code = self.chunk.codes[self.ip];
            self.ip += 1;
            OpCode::from_u8(code)
        } else {
            None
        }
    }

    fn read_index(&mut self, err: &'static str) -> Result<usize> {
        let idx = self.read_byte().context(err)? as usize;
        Ok(self.slots + idx)
    }
}

#[derive(Debug)]
struct Vm {
    stack: Vec<Value>,
    strings: Strings,
    objects: Option<Object>,
    globals: Table,
}

impl Vm {
    fn new() -> Self {
        Vm {
            strings: Strings::new(),
            stack: vec![],
            objects: None,
            globals: Table::new(),
        }
    }

    fn interpreter(&mut self, source: &str) -> Result<()> {
        let mut compiler = compile(source)?;
        let func = compiler.func;
        println!("funCompiler: {}", func);
        let fun = Rc::new(ObjFunction {
            chunk: func.chunk,
            name: func.name,
            arity: func.arity,
        });
        self.push(Value::ObjFunction(fun.clone()));
        let result = self.call(fun.clone(), 0);
        if result.is_err() {
            println!("fun: {:?}", fun);
            println!("vm: {:?}", self);
        }
        result
    }

    fn call(&mut self, fun: Rc<ObjFunction>, arg_count: usize) -> Result<()> {
        println!("call fun: {}", fun.get_name());
        let mut frame = CallFrame {
            chunk: &fun.chunk,
            ip: 0,
            slots: self.stack.len() - arg_count - 1,
        };
        while let Some(code) = frame.read_byte() {
            println!("code: {:?}", code);
            self.run(code, &mut frame)?;
        }
        Ok(())
    }

    fn run(&mut self, code: OpCode, frame: &mut CallFrame) -> Result<()> {
        match code {
            Nil => self.push(Value::Nil),
            True => self.push(Value::Boolean(true)),
            False => self.push(Value::Boolean(false)),
            Not => {
                let value = self.pop()?.as_bool()?;
                self.push(Value::Boolean(!value))
            }
            Return => {
                println!("{:?}", self.pop());
            }
            Const => {
                let c = frame.read_const();
                self.push(c);
            }
            Negate => {
                let result = self.pop_num()?;
                self.push_num(-result)
            }
            Add => match self.peek() {
                Some(Value::Number(_)) => {
                    let right = self.pop_num()?;
                    let left = self.pop_num()?;
                    self.push_num(left + right)
                }
                Some(Value::ObjString(_)) => {
                    let right = self.pop()?.as_str()?;
                    let left = self.pop()?.as_str()?;
                    let result = unsafe { format!("{}{}", (*left).data, (*right).data) };
                    let key = self.strings.add(result);
                    self.push(Value::ObjString(key));
                }
                v @ _ => bail!("unsupport add for value: {:?}", v),
            },
            Substract => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push_num(left - right)
            }
            Multiply => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push_num(left * right)
            }
            Divide => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push_num(left / right)
            }
            Print => {
                let val = self.pop()?;
                match val {
                    Value::ObjString(key) => {
                        println!("===> {:?}", unsafe { &*key })
                    }
                    _ => {
                        println!("===> {:?}", val)
                    }
                }
            }
            Pop => {
                self.pop()?;
            }
            DefineGlobal => {
                let name = frame.read_string()?;
                let val = self.pop()?;
                self.globals.set(name, val);
            }
            GetGlobal => {
                let name = frame.read_string()?;
                let val = self
                    .globals
                    .get(name)
                    .context(format!("undefined variable {:?}", name))?
                    .clone();
                self.push(val);
            }
            SetGlobal => {
                let name = frame.read_string()?;
                let val = self.peek().context("missing value for SetGlobal")?.clone();
                if self.globals.set(name, val) {
                    bail!("Undefined variable '{:?}'", unsafe { &*name })
                }
            }
            GetLocal => {
                let slot = frame.read_index("GetLocal need index")?;
                let value = self.stack.get(slot).context("Can't find local")?.clone();
                self.push(value)
            }
            SetLocal => {
                let slot = frame.read_index("SetLocal need index")?;
                self.stack[slot] = self.peek().context("Set Local need value")?.clone();
            }
            JumpIfFalse => {
                let offset = frame.read_u16();
                let condition = self.pop()?;
                if condition.is_false() {
                    frame.ip += offset as usize
                }
            }
            JumpAndFalse => {
                let offset = frame.read_u16();
                let condition = self.peek().context("JumpAndFalse need value")?;
                if condition.is_false() {
                    frame.ip += offset as usize
                }
            }
            JumpOrTrue => {
                let offset = frame.read_u16();
                let condition = self.peek().context("JumpOrTrue need value")?;
                if !condition.is_false() {
                    frame.ip += offset as usize
                }
            }
            Jump => {
                let offset = frame.read_u16();
                frame.ip += offset as usize
            }
            Call => {
                let arg_count = frame.read_byte().context("requrie arg count")? as usize;
                let callee = self
                    .peek_num(arg_count)
                    .context("need callee value")?
                    .clone();
                self.call_value(&callee, arg_count)?;
            }
        }
        Ok(())
    }

    fn call_value(&mut self, callee: &Value, arg_count: usize) -> Result<()> {
        match callee {
            Value::ObjFunction(fun) => self.call(fun.clone(), arg_count),
            _ => bail!("Can only call functions and classes!"),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn push_num(&mut self, num: f64) {
        self.push(Value::Number(num))
    }

    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().context("emtpy while pop!")
    }

    fn peek(&self) -> Option<&Value> {
        self.stack.last()
    }

    fn peek_num(&self, num: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - num - 1)
    }

    fn pop_num(&mut self) -> Result<f64> {
        self.pop()?.as_num()
    }

    fn debug(&self) {
        for v in &self.stack {
            print!("[{:?}]", v);
        }
    }
}

#[cfg(test)]
#[path = "./vm_test.rs"]
mod vm_test;
