use anyhow::Result;
use std::{fmt::{Display, write}, ptr};

use super::chunk::OpCode;
use super::{
    chunk::Chunk,
    compiler::{Compiler, FunCompiler, Local},
    object::{ObjFunction, ObjString},
    value::Value,
};

pub fn fmt_func(func: &FunCompiler, f: &mut std::fmt::Formatter<'_>) -> Result<()> {
    if func.name == ptr::null() {
        write!(f, "name: __script__")?;
    }else {
        write!(f, "name: {:?}", unsafe {&*func.name})?;
    }
    writeln!(f, ", artiy: {}, scope_depth: {}", func.arity, func.scope_depth)?;
    fmt_locals(&func.locals, f)?;
    fmt_chunk(&func.chunk, f)?;
    Ok(())
}

pub fn fmt_fun(func: &ObjFunction, f: &mut std::fmt::Formatter<'_>) -> Result<()> {
    write!(f, "name: {:?}, artiy: {}", unsafe { &*func.name }, func.arity)?;
    fmt_chunk(&func.chunk, f)?;
    Ok(())
}
fn fmt_locals(locals: &Vec<Local>, f: &mut std::fmt::Formatter<'_>) -> Result<()> {
    for (idx, local) in locals.iter().enumerate() {
        write!(f, "local: {} ==> {:?}", idx, local)?;
    }
    Ok(())
}

fn fmt_chunk(chunk: &Chunk, f: &mut std::fmt::Formatter<'_>) -> Result<()> {
    let mut idx = 0usize;
    while idx < chunk.codes.len() {
        idx = fmt_code(chunk, idx, f)?;
    }
    Ok(())
}
fn fmt_code(chunk: &Chunk, idx: usize, f: &mut std::fmt::Formatter<'_>) -> Result<usize> {
    use OpCode::*;
    let lines = &chunk.lines;
    let consts = &chunk.consts;
    let codes = &chunk.codes;
    let mut idx = idx;
    write!(f, "{:0>4}", idx)?;
    if idx > 0 && lines[idx] == lines[idx - 1] {
        write!(f, "    | ")?;
    } else {
        write!(f, "{:>4} ", lines[idx])?;
    }
    let byte = codes[idx];
    let code = OpCode::from_u8(byte).unwrap();
    match code {
        Return => writeln!(f, "OP_RETURN")?,
        Const => {
            idx += 1;
            let const_idx = codes[idx];
            writeln!(f, "OP_CONSTANT: {}", consts[const_idx as usize])?;
        }
        Nil | True | False | Pop => {
            writeln!(f, "{:?}", code)?;
        }
        DefineGlobal | GetGlobal | SetGlobal | GetLocal | SetLocal => {
            idx += 1;
            let const_idx = codes[idx];
            writeln!(f, "{:?}:", code )?;
            fmt_value(&consts[const_idx as usize], f)?;
        }
        Negate => writeln!(f, "OP_Negate")?,
        _ => writeln!(f, "{:?}", code)?,
    }
    Ok(idx + 1)
}

fn fmt_value(value: &Value, f: &mut std::fmt::Formatter<'_>) -> Result<()> {
    match value {
        Value::ObjString(s) => writeln!(f, "{:?}", unsafe{&**s})?,
        Value::ObjFunction(fun) => fmt_fun(fun, f)?,
        _ => writeln!(f, "{:?}", value)?,
    }
    Ok(())
}
