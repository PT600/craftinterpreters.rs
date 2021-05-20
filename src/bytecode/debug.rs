use super::{chunk::Chunk, compiler::{Compiler, FunCompiler, Local}, object::{ObjFunction, ObjString}, strings::Strings, value::Value};
use super::chunk::OpCode;

pub fn disassemble(func: &FunCompiler) {
  disassemble_fun(func.name, &func.chunk);
  disassemble_locals(&func.locals);
}

pub fn disassemble_locals(locals: &Vec<Local>) {
  for (idx, local) in locals.iter().enumerate() {
    println!("local: {} ==> {:?}", idx, local)
  }
}
pub fn disassemble_fun(name: *const ObjString, chunk: &Chunk) {
    println!("==fun: {:?} ==", unsafe{&*name});
    let mut idx = 0usize;
    while idx < chunk.codes.len() {
        idx = disassemble_code(chunk, idx)
    }
}

fn disassemble_value(value: &Value) {
  
}

fn disassemble_code(chunk: &Chunk, idx: usize) -> usize {
    use OpCode::*;
    let lines = &chunk.lines;
    let consts = &chunk.consts;
    let codes = &chunk.codes;
    let mut idx = idx;
    print!("{:0>4}", idx);
    if idx > 0 && lines[idx] == lines[idx - 1] {
        print!("    | ")
    } else {
        print!("{:>4}", lines[idx])
    }
    let byte = codes[idx];
    let code = OpCode::from_u8(byte).unwrap();
    match code {
        Return => println!("OP_RETURN"),
        Const => {
            idx += 1;
            let const_idx = codes[idx];
            println!("OP_CONSTANT: {}", consts[const_idx as usize]);
        }
        Nil | True | False | Pop => {
            println!("{:?}", code)
        }
        DefineGlobal | GetGlobal | SetGlobal | GetLocal | SetLocal => {
            idx += 1;
            let const_idx = codes[idx];
            println!("{:?}: {}", code, consts[const_idx as usize]);
        }
        Negate => println!("OP_Negate"),
        _ =>  println!("{:?}", code) 
    }
    idx + 1
}
