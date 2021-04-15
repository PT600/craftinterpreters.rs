use std::usize;

use anyhow::{bail, Context, Result};

use super::{compiler, object::Object, strings::Strings};

use super::value::Value;
use super::{
    chunk::{
        Chunk,
        OpCode::{self, *},
    },
};

const STACK_MAX: usize = 256;

struct Vm {
    ip: usize,
    stack: Vec<Value>,
    strings: Strings,
    objects: Option<Object>,
}

impl Vm {
    fn new(strings: Strings) -> Self {
        Vm {
            ip: 0,
            stack: vec![],
            strings,
            objects: None,
        }
    }
    fn run(&mut self, chunk: &Chunk) -> Result<()> {
        while let Some(code) = self.read_byte(chunk) {
            println!("code: {:?}", code);
            self.step(code, chunk)?
        }
        Ok(())
    }

    fn step(&mut self, code: OpCode, chunk: &Chunk) -> Result<()> {
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
                let idx = self.read_byte(chunk).context("missing byte")? as usize;
                let c = chunk.read_const(idx);
                self.push(c);
            }
            Negate => {
                let result = self.pop_num()?;
                self.push_num(-result)
            }
            Add => {
                match self.peek(){
                    Some(Value::Number(_)) => {
                        let right = self.pop_num()?;
                        let left = self.pop_num()?;
                        self.push_num(left + right)
                    }
                    Some(Value::ObjString(_)) => {
                        let right = self.pop()?.as_str()?;
                        let left = self.pop()?.as_str()?;
                        let result = unsafe{ format!("{}{}", (*left).data, (*right).data)};
                        let key = self.strings.add(&result);
                        self.push(Value::ObjString(key));
                    }
                    v @ _ => bail!("unsupport add for value: {:?}", v)
                }
            }
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
        }
        Ok(())
    }

    fn read_const(&mut self, chunk: Chunk) -> Value {
        assert!(self.ip < chunk.codes.len(), "array out of bound!");
        let idx = chunk.codes[self.ip];
        self.ip += 1;
        chunk.consts[idx as usize].clone()
    }

    fn read_byte(&mut self, chunk: &Chunk) -> Option<OpCode> {
        if self.ip < chunk.codes.len() {
            let code = chunk.codes[self.ip];
            self.ip += 1;
            OpCode::from_u8(code)
        } else {
            None
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

    fn pop_num(&mut self) -> Result<f64> {
        self.pop()?.as_num()
    }

    fn debug(&self) {
        for v in &self.stack{
            print!("[{:?}]", v);
        }
    }
}

#[cfg(test)] 
mod tests{
    use compiler::Compiler;

    use super::*;

    fn assert_eq(source: &str, value: Value) {
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(source).unwrap();
        let mut vm = Vm::new(compiler.strings);
        let result = vm.run(&chunk);
        println!("result {:?}, stack: {:?}", result, vm.stack);
        match result {
            Ok(()) => assert!(matches!(vm.stack.pop(), Some(value))),
            Err(err) => {
                println!("error, {:?}", err);
                assert!(false, "err!");
            }
        }
    }

    #[test]
    fn test() {
        assert_eq("1+1 -2*3", Value::Number(-4.0));
    }
    // #[test]
    // fn add_str(){
    //     assert_eq("\"abc\" + \"efg\"", Value::Str("abcefg".into()));
    // }
}
