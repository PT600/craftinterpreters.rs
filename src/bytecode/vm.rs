use std::usize;

use anyhow::{Context, Result, bail};

use super::chunk::{
    Chunk,
    OpCode::{self, *},
    Value,
};
use super::compiler::*;

const STACK_MAX: usize = 256;

#[derive(Default)]
struct Vm {
    ip: usize,
    stack: [Value; 32],
    stack_top: usize,
}

impl Vm {
    fn run(&mut self, chunk: &Chunk) -> Result<()> {
        while let Some(code) = self.read_byte(chunk) {
          println!("code: {:?}", code);
            self.step(code, chunk)?
        }
        Ok(())
    }

    fn step(&mut self, code: OpCode, chunk: &Chunk) -> Result<()> {
        match code {
            Return => {
                println!("{}", self.pop());
            }
            Const => {
              let idx = self.read_byte(chunk).context("missing byte")? as usize;
              let c = chunk.read_const(idx);
              self.push(c);
            }
            Negate => {
                let result = -self.pop();
                self.push(result)
            }
            Add => {
              let right = self.pop();
              let left = self.pop();
              self.push(left + right)
            }
            Substract => {
              let right = self.pop();
              let left = self.pop();
              self.push(left - right)
            }
            Multiply => {
              let right = self.pop();
              let left = self.pop();
              self.push(left * right)
            }
            Divide => {
              let right = self.pop();
              let left = self.pop();
              self.push(left / right)

            }
        }
        Ok(())
    }

    fn read_const(&mut self, chunk: &Chunk) -> Value {
        assert!(self.ip < chunk.codes.len(), "array out of bound!");
        let idx = chunk.codes[self.ip];
        self.ip += 1;
        chunk.consts[idx as usize]
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
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    fn debug(&self) {
        for idx in 0..self.stack_top {
            print!("[{}]", self.stack[idx]);
        }
    }
}


#[test]
fn test(){
  let compiler = Compiler::compile("1+1 -2*3");
  let mut vm = Vm::default();
  vm.run(&compiler.chunk);
  println!("{:?}", vm.stack);
}