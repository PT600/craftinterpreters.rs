use std::cell::RefCell;
use std::fmt::Result;
use std::rc::Rc;
use std::usize;
use std::{
    fmt::{write, Display},
    ptr,
};

use super::chunk::OpCode;
use super::object::ObjClosure;
use super::{
    chunk::Chunk,
    compiler::{Compiler, FunCompiler, Local},
    object::{ObjFunction, ObjString},
    value::Value,
};

pub fn fmt_func(func: &FunCompiler, f: &mut std::fmt::Formatter<'_>) -> Result {
    if func.name == ptr::null() {
        write!(f, "name: __script__")?;
    } else {
        write!(f, "name: {:?}", (unsafe { &*func.name }).data)?;
    }
    writeln!(
        f,
        ", artiy: {}, scope_depth: {}",
        func.arity, func.scope_depth
    )?;
    fmt_locals(&func.locals, f)?;
    fmt_chunk(&func.chunk, f)?;
    Ok(())
}

pub fn fmt_closure(closure: Rc<RefCell<ObjClosure>>, f: &mut std::fmt::Formatter<'_>) -> Result {
    write!(f, "closure: ")?;
    fmt_fun(&closure.borrow().fun, f)
}
pub fn fmt_fun(fun: &ObjFunction, f: &mut std::fmt::Formatter<'_>) -> Result {
    writeln!(f, "fun, name: {:?}, artiy: {}", fun.get_name(), fun.arity)?;
    fmt_chunk(&fun.chunk, f)?;
    Ok(())
}
fn fmt_locals(locals: &Vec<Local>, f: &mut std::fmt::Formatter<'_>) -> Result {
    for (idx, local) in locals.iter().enumerate() {
        writeln!(f, "local: {} ==> {:?}", idx, local)?;
    }
    Ok(())
}

pub fn fmt_chunk(chunk: &Chunk, f: &mut std::fmt::Formatter<'_>) -> Result {
    writeln!(f, "chunk.codes:")?;
    let mut idx = 0usize;
    while idx < chunk.codes.len() {
        fmt_code(chunk, &mut idx, f)?;
    }
    writeln!(f, "chunk.consts:")?;
    for c in &chunk.consts {
        writeln!(f, "const: {}", c)?;
    }
    Ok(())
}

fn fmt_code(chunk: &Chunk, idx_mut: &mut usize, f: &mut std::fmt::Formatter<'_>) -> Result {
    use OpCode::*;
    let idx = *idx_mut;
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
        DefineGlobal | GetGlobal | SetGlobal | GetLocal | SetLocal | Call | Closure
        | GetUpvalue | SetUpvalue => {
            idx += 1;
            let index = codes[idx];
            writeln!(f, "code: {:?}, index: {}", code, index)?;
            if matches!(code, Closure) {
                writeln!(f, "===>fun, start upvalues: ")?;
                if let Value::ObjFunction(fun) = chunk.read_const(index as usize) {
                    for _ in 0..fun.upvalue_count {
                        idx += 1;
                        write!(f, "===>index: {}", codes[idx])?;
                        idx += 1;
                        write!(f, ", local: {}", codes[idx])?;
                    }
                }
                writeln!(f, "\n===>fun, end upvalues: ")?;
            }
        }
        Negate => writeln!(f, "OP_Negate")?,
        _ => writeln!(f, "{:?}", code)?,
    }
    *idx_mut = idx + 1;
    Ok(())
}
