use anyhow::{bail, Context, Result};
use std::{
    cell::RefCell,
    collections::hash_map::DefaultHasher,
    fmt::Display,
    hash::{Hash, Hasher},
    ptr,
    rc::Rc,
    usize,
};

use super::{chunk::Chunk, debug, value::Value};

#[derive(Debug)]
pub struct Object {
    pub next: Option<Rc<Object>>,
    pub obj: Obj,
}

impl Object {
    pub fn is_str(&self) -> bool {
        return matches!(self.obj, Obj::String(_));
    }
    pub fn as_str(&self) -> Result<*const ObjString> {
        match self.obj {
            Obj::String(obj) => Ok(obj),
            _ => bail!("can't convert {:?} to str!", self),
        }
    }
}

#[derive(Debug)]
pub enum Obj {
    String(*const ObjString),
    Function(ObjFunction),
    NativeFun,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ObjString {
    pub data: String,
    pub hash: usize,
}

#[derive(Debug, Clone)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub upvalue_count: usize,
    pub name: *const ObjString,
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Default::default(),
            upvalue_count: 0,
            name: ptr::null(),
        }
    }

    pub fn get_name(&self) -> &str {
        if self.name == ptr::null() {
            "__script__"
        } else {
            &(unsafe { &*self.name }).data
        }
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug::fmt_fun(self, f)
    }
}

impl ObjString {
    pub fn new(data: &str) -> Self {
        let data = data.to_string();
        let hash = Self::hash(&data);
        Self { data, hash }
    }

    pub fn hash(key: &str) -> usize {
        let mut s = DefaultHasher::new();
        key.hash(&mut s);
        let result = s.finish();
        result as usize
    }
}

#[derive(Debug)]
pub struct ObjUpvalue {
    pub next: Option<Box<ObjUpvalue>>,
    pub closed_value: Option<Value>,
    pub value: *mut Value,
    pub location: usize,
}
#[derive(Debug)]
pub struct ObjClosure {
    pub fun: Rc<ObjFunction>,
    pub upvalues: Vec<*mut ObjUpvalue>,
    pub upvalue_count: usize,
}

impl ObjClosure {
    pub fn new(fun: Rc<ObjFunction>) -> Self {
        let upvalue_count = fun.upvalue_count;
        Self {
            fun,
            upvalues: vec![],
            upvalue_count,
        }
    }
}
