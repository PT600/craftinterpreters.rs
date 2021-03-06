use std::{cell::RefCell, collections::HashMap, fmt::Display, mem, ptr, rc::Rc, usize};

use anyhow::{bail, Context, Result};

use super::{
    compiler::compile,
    object::{Obj, ObjClosure, ObjFunction, ObjString, ObjUpvalue, Object},
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

#[derive(Debug)]
struct CallFrame<'a> {
    chunk: &'a Chunk,
    closure: Rc<RefCell<ObjClosure>>,
    ip: usize,
    slots: usize,
}

impl<'a> Display for CallFrame<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ip: {}, slots: {}", self.ip, self.slots)?;
        debug::fmt_chunk(&self.chunk, f)
    }
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

    fn read_code(&mut self) -> Option<OpCode> {
        if self.ip < self.chunk.codes.len() {
            let code = self.chunk.codes[self.ip];
            self.ip += 1;
            OpCode::from_u8(code)
        } else {
            None
        }
    }

    fn read_index(&mut self, err: &'static str) -> Result<usize> {
        let idx = self.read_code().context(err)? as usize;
        Ok(self.slots + idx)
    }

    fn read_byte(&mut self, err: &'static str) -> Result<u8> {
        let byte = self.read_code().context(err)? as u8;
        Ok(byte)
    }

    fn read_bool(&mut self, err: &'static str) -> Result<bool> {
        let value = self.read_byte(err)? == 1;
        Ok(value)
    }
}

#[derive(Debug)]
struct Vm {
    stack: Vec<Value>,
    strings: Strings,
    objects: Option<Object>,
    globals: Table,
    open_upvalues: Option<Box<ObjUpvalue>>,
}

impl Vm {
    fn new() -> Self {
        Vm {
            strings: Strings::new(),
            stack: vec![],
            objects: None,
            globals: Table::new(),
            open_upvalues: None,
        }
    }

    fn interpreter(&mut self, source: &str) -> Result<()> {
        let compiler = compile(source)?;
        let func = compiler.func;
        println!("funCompiler: {}", func);
        let fun = Rc::new(ObjFunction {
            chunk: func.chunk,
            name: func.name,
            upvalue_count: 0,
            arity: func.arity,
        });
        let closure = Rc::new(RefCell::new(ObjClosure::new(fun.clone())));
        self.push(Value::ObjClosure(closure.clone()));
        self.call(closure, 0)
    }

    fn call(&mut self, closure: Rc<RefCell<ObjClosure>>, arg_count: usize) -> Result<()> {
        let mut frame = CallFrame {
            chunk: &closure.borrow().fun.chunk,
            closure: closure.clone(),
            ip: 0,
            slots: self.stack.len() - arg_count - 1,
        };
        let mut return_value = Value::Nil;
        while let Some(code) = frame.read_code() {
            println!("run code: {:?}", code);
            if matches!(code, Return) {
                // neccessary?
                self.close_upvalues(frame.slots);
                return_value = self.pop()?;
                break;
            } else {
                self.run(&code, &mut frame)?;
            }
        }
        self.stack.drain(frame.slots..);
        // let pop_count = self.stack.len() - frame.slots;
        // for _ in 0..pop_count {
        //     self.pop()?;
        // }
        self.push(return_value);
        Ok(())
    }

    fn run(&mut self, code: &OpCode, frame: &mut CallFrame) -> Result<()> {
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
            Add => match self.peek_num(1) {
                Some(Value::Number(_)) => {
                    let right = self.pop_num()?;
                    let left = self.pop_num()?;
                    self.push_num(left + right)
                }
                Some(Value::ObjString(_)) => {
                    let right = self.pop()?.as_str()?;
                    let left = self.pop()?.as_str()?;
                    let result = format!("{}{}", left, right);
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
            EqualEqual => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push(Value::Boolean(left == right))
            }
            BangEqual => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push(Value::Boolean(left != right))
            }
            Greater => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push(Value::Boolean(left > right))
            }
            GreaterEqual => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push(Value::Boolean(left >= right))
            }
            Less => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push(Value::Boolean(left < right))
            }
            LessEqual => {
                let right = self.pop_num()?;
                let left = self.pop_num()?;
                self.push(Value::Boolean(left <= right))
            }
            Print => {
                let val = self.pop()?;
                println!("$===> {}", val)
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
                    .context(format!("GetGlobal: undefined variable {:?}", unsafe {
                        &*name
                    }))?
                    .clone();
                self.push(val);
            }
            SetGlobal => {
                let name = frame.read_string()?;
                let val = self.peek().context("missing value for SetGlobal")?.clone();
                if self.globals.set(name, val) {
                    bail!("SetGlobal Undefined variable '{:?}'", unsafe { &*name })
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
            GetUpvalue => {
                let slot = frame.read_byte("GetUpvalue need index")? as usize;
                let upvalue = unsafe { &*frame.closure.borrow().upvalues[slot] };
                let value = unsafe { &*upvalue.value }.clone();
                self.push(value)
            }
            SetUpvalue => {
                let slot = frame.read_byte("SetUpvalue need index")? as usize;
                let value = self.peek().context("need value to SetUpvalue")?.clone();
                let upvalue = unsafe { &*frame.closure.borrow().upvalues[slot] };
                let target = unsafe { &mut *upvalue.value };
                let _ = std::mem::replace(target, value);
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
                let arg_count = frame.read_code().context("requrie arg count")? as usize;
                let callee = self
                    .peek_num(arg_count)
                    .context("need callee value")?
                    .clone();
                self.call_value(&callee, arg_count)?;
            }
            Closure => match frame.read_const() {
                Value::ObjFunction(fun) => {
                    let mut closure = ObjClosure::new(fun);
                    for i in 0..closure.fun.upvalue_count {
                        let index = frame.read_byte("upvalue.index is missing")?;
                        let local = frame.read_bool("upvalue.local is missing")?;
                        if local {
                            let location = frame.slots + index as usize;
                            let value = &mut self.stack[location] as *mut Value;
                            let upvalue = self.capture_upvalue(value, location);
                            closure.upvalues.push(upvalue);
                        } else {
                            let upvalue = frame.closure.borrow().upvalues[index as usize].clone();
                            closure.upvalues.push(upvalue)
                        }
                    }
                    self.push(Value::ObjClosure(Rc::new(RefCell::new(closure))))
                }
                v @ _ => bail!("need fun for closure, got {:?}", v),
            },
            CloseUpvalue => {
                let location = self.stack.len() - 1;
                // let mut head = &mut self.open_upvalues;
                // Vm::close_upvalue(val, location, head)
                self.close_upvalues(location);
                let val = self.pop()?;
            }
        }
        Ok(())
    }

    fn close_upvalues(&mut self, location: usize) {
        while let Some(node) = &mut self.open_upvalues {
            if node.location >= location {
                node.closed_value = Some(unsafe { &*node.value }.clone());
                node.value = node.closed_value.as_mut().unwrap();
                self.open_upvalues = node.next.take();
            } else {
                break;
            }
        }
    }
    fn close_upvalue(value: Value, location: usize, node: &mut Option<Box<ObjUpvalue>>) {
        if let Some(node) = node {
            if node.location == location {
                node.closed_value = Some(value);
                node.value = node.closed_value.as_mut().unwrap();
            } else {
                Vm::close_upvalue(value, location, &mut node.next)
            }
        }
    }

    fn capture_upvalue(&mut self, value: *mut Value, location: usize) -> *mut ObjUpvalue {
        let mut prev_upvalue = ptr::null_mut() as *mut Box<ObjUpvalue>;
        let mut upvalue = &mut self.open_upvalues;
        while let Some(upv) = upvalue {
            if upv.location > location {
                prev_upvalue = &mut *upv;
                upvalue = &mut upv.next;
            } else if upv.location == location {
                return &mut **upv;
            } else {
                break;
            }
        }
        let mut created_upvalue = Box::new(ObjUpvalue {
            next: None,
            closed_value: None,
            value,
            location,
        });
        let result = &mut *created_upvalue as *mut _;
        if prev_upvalue == ptr::null_mut() {
            self.open_upvalues = Some(created_upvalue);
        } else {
            let prev_upvalue = unsafe { &mut *prev_upvalue };
            created_upvalue.next = prev_upvalue.next.take();
            prev_upvalue.next.replace(created_upvalue);
        }
        result
    }

    fn call_value(&mut self, callee: &Value, arg_count: usize) -> Result<()> {
        match callee {
            Value::ObjClosure(closure) => self.call(closure.clone(), arg_count),
            _ => bail!("Can only call functions and classes, {:?}", callee),
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
        self.stack.get(self.stack.len() - 1 - num)
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
