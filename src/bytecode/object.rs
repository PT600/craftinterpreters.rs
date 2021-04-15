use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    rc::Rc,
    usize,
};
use anyhow::{Result, bail};

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    String(*const ObjString),
    Function,
    NativeFun,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ObjString {
    pub data: String,
    pub hash: usize,
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
