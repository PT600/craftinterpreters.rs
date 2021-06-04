use super::object::ObjString;
use super::value::Value;
use std::fmt::Display;
use std::{mem, usize};

pub type Key = *const ObjString;

#[derive(Debug, Clone)]
pub struct Entry {
    key: *const ObjString,
    value: Value,
}

impl Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let key = unsafe { &*self.key };
        writeln!(f, "key: {:?}, value: {}", key, self.value)
    }
}
#[derive(Default, Debug)]
pub struct Table {
    tombstones: Vec<bool>,
    entries: Vec<Option<Entry>>,
    count: usize,
    mask: usize,
}
impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "table.entries: ")?;
        for (index, entry) in self.entries.iter().enumerate() {
            if let Some(entry) = entry {
                writeln!(f, "{}+{} => {}", index, self.tombstones[index], entry)?;
            }
        }
        Ok(())
    }
}

enum Search {
    Empty(usize),
    Exists(usize),
}

impl Table {
    pub const MAX_LOAD: f32 = 0.75;
    pub fn new() -> Self {
        let capacity = 8;
        Self {
            entries: vec![None; capacity],
            count: 0,
            tombstones: vec![false; capacity],
            mask: capacity - 1,
        }
    }
    fn hash(&self, key: Key) -> usize {
        unsafe { &*key }.hash & self.mask
    }
    fn search(&self, key: Key) -> Search {
        let mut index = self.hash(key);
        let mut tombstone = None;
        loop {
            match &self.entries[index] {
                None => {
                    if !self.tombstones[index] {
                        let index = tombstone.unwrap_or(index);
                        return Search::Empty(index);
                    } else if tombstone == None {
                        tombstone = Some(index);
                    }
                }
                Some(entry) => {
                    if unsafe { &*entry.key }.data == unsafe { &*key }.data {
                        return Search::Exists(index);
                    }
                }
            }
            index = index.wrapping_add(1) & self.mask;
        }
    }

    pub fn get<'a>(&'a self, key: Key) -> Option<&'a Value> {
        let mut index = self.hash(key);
        loop {
            match &self.entries[index] {
                None if !self.tombstones[index] => return None,
                Some(entry) if entry.key == key => return Some(&entry.value),
                _ => {}
            }
            index = index.wrapping_add(1) & self.mask;
        }
    }

    pub fn set(&mut self, key: Key, value: Value) -> bool {
        if (self.count + 1) as f32 > (self.entries.capacity() as f32) * Self::MAX_LOAD {
            self.adjust_capacity();
        }
        match self.search(key) {
            Search::Empty(idx) => {
                self.entries[idx].replace(Entry { key, value });
                if !self.tombstones[idx] {
                    self.count += 1;
                }
                true
            }
            Search::Exists(idx) => {
                self.entries[idx].replace(Entry { key, value });
                false
            }
        }
    }

    pub fn delete(&mut self, key: Key) {
        if let Search::Exists(idx) = self.search(key) {
            self.tombstones[idx] = true;
            let _ = mem::replace(&mut self.entries[idx], None);
        }
    }

    pub fn adjust_capacity(&mut self) {
        self.entries.reserve_exact(self.entries.capacity());
        let _ = mem::replace(&mut self.tombstones, vec![false; self.entries.capacity()]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut table = Table::new();
        let a = ObjString::new("a");
        let b = ObjString::new("b");
        let c = ObjString::new("c");
        table.set(&a, Value::Boolean(true));
        table.set(&b, Value::Number(1.0));
        table.set(&c, Value::Nil);
        assert_eq!(table.count, 3);
        table.delete(&a);
        assert!(table.get(&a).is_none());
        table.set(&a, Value::Boolean(false));
        assert!(table.get(&a).is_some());
    }

    #[test]
    fn cmp_by_ref() {
        let mut table = Table::new();
        let a1 = ObjString::new("a");
        let a2 = ObjString::new("a");
        table.set(&a1, Value::Number(1.0));
        table.set(&a2, Value::Number(2.0));
        assert_eq!(table.count, 2);
    }

    #[test]
    fn cmp() {
        let a = "a".to_string();
        let b = "a".to_string();
        assert_eq!(a, b);
        assert!(!std::ptr::eq(&a, &b));
    }
}
