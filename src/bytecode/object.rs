use anyhow::{bail, Result, Context};
use std::{collections::hash_map::DefaultHasher, fmt::Display, hash::{Hash, Hasher}, ptr, rc::Rc, usize};

use super::{chunk::Chunk, debug};

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

#[derive(Debug, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: *const ObjString,
}

impl ObjFunction {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Default::default(),
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
