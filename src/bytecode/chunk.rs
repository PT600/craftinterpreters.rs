use std::fmt::Display;

use super::value::Value;
use num;
use num_derive::FromPrimitive;

#[derive(Debug, Clone, FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Nil,
    True,
    False,
    Const,
    Not,
    Negate,
    Add,
    Substract,
    Multiply,
    Divide,
    Return,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    JumpAndFalse,
    JumpOrTrue,
    Jump,
    Call,
}

impl OpCode {
    pub fn from_u8(val: u8) -> Option<Self> {
        num::FromPrimitive::from_u8(val)
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Chunk {
    pub codes: Vec<u8>,
    pub lines: Vec<usize>,
    pub consts: Vec<Value>,
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "codes:");
    }
}

impl Chunk {
    pub fn write(&mut self, code: OpCode, line: usize) {
        self.codes.push(code as u8);
        self.lines.push(line);
    }

    pub fn write_byte(&mut self, code: u8, line: usize) {
        self.codes.push(code);
        self.lines.push(line);
    }

    pub fn write_const(&mut self, value: Value, line: usize) {
        self.write(OpCode::Const, line);
        let idx = self.add_const(value);
        self.codes.push(idx as u8);
        self.lines.push(line);
    }

    pub fn add_const(&mut self, value: Value) -> usize {
        self.consts.push(value);
        self.consts.len() - 1
    }

    pub fn read_const(&self, idx: usize) -> Value {
        self.consts[idx].clone()
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut idx = 0usize;
        while idx < self.codes.len() {
            idx = self.disassemble_code(self.codes[idx], idx)
        }
    }

    fn disassemble_code(&self, code: u8, idx: usize) -> usize {
        use OpCode::*;
        let mut idx = idx;
        print!("{:0>4}", idx);
        if idx > 0 && self.lines[idx] == self.lines[idx - 1] {
            print!("    | ")
        } else {
            print!("{:>4}", self.lines[idx])
        }
        let code = OpCode::from_u8(code).unwrap();
        match code {
            Return => println!("OP_RETURN"),
            Const => {
                idx += 1;
                let const_idx = self.codes[idx];
                println!("OP_CONSTANT: {}", self.consts[const_idx as usize]);
            }
            Negate => println!("OP_Negate"),
            _ => {}
        }
        idx + 1
    }
}

#[cfg(test)]
mod tests {
    use super::OpCode::*;
    use super::*;

    #[test]
    fn test() {
        println!("{}", Negate as u8);
        let mut chunk: Chunk = Default::default();
        let val = chunk.add_const(Value::Number(1.2));
        chunk.write(Const, 123);
        chunk.write(Negate, 123);
        chunk.write(Return, 123);
    }
}
