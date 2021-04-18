use std::{mem, usize};

use super::object::ObjString;

#[derive(Default, Debug)]
pub struct Strings {
    tombstones: Vec<bool>,
    entries: Vec<Option<ObjString>>,
    count: usize,
    mask: usize,
}

impl Strings {
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

    pub fn get<'a>(&'a self, key: &str) -> Option<&'a ObjString> {
        let mut index = ObjString::hash(key) & self.mask;
        loop {
            match &self.entries[index] {
                None if !self.tombstones[index] => return None,
                Some(entry) if entry.data == key => return Some(entry),
                _ => {}
            }
            index = index.wrapping_add(1) & self.mask;
        }
    }

    pub fn add(&mut self, key: String) -> *const ObjString {
        if (self.count + 1) as f32 > (self.entries.capacity() as f32) * Self::MAX_LOAD {
            self.adjust_capacity();
        }
        let hash = ObjString::hash(&key);
        let mut index = hash & self.mask;
        let mut tombstone = None;
        loop {
            match &self.entries[index] {
                None => {
                    if !self.tombstones[index] {
                        let index = match tombstone {
                            None => {
                                self.count += 1;
                                index
                            }
                            Some(index) => index,
                        };
                        let entry = ObjString {
                            data: key,
                            hash,
                        };
                        let o = &mut self.entries[index];
                        o.replace(entry);
                        return o.as_ref().unwrap();
                    } else if tombstone == None {
                        tombstone = Some(index);
                    }
                }
                Some(entry) => {
                    if entry.data == *key {
                        return entry;
                    }
                }
            }
            index = index.wrapping_add(1) & self.mask;
        }
    }

    fn delete(&mut self, key: &str) -> Option<ObjString> {
        let mut index = ObjString::hash(key) & self.mask;
        loop {
            match &self.entries[index] {
                None if !self.tombstones[index] => return None,
                Some(entry) if entry.data == key => {
                    let result = mem::replace(&mut self.entries[index], None);
                    self.tombstones[index] = true;
                    return result;
                }
                _ => {}
            }
            index = index.wrapping_add(1) & self.mask;
        }
    }

    fn adjust_capacity(&mut self) {
        self.entries.reserve_exact(self.entries.capacity());
        let _ = mem::replace(&mut self.tombstones, vec![false; self.entries.capacity()]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn testt() {
        let mut strings = Strings::new();
        let key = "a";
        strings.add(key);
        strings.add("b");
        strings.add("c");
        assert_eq!(strings.count, 3);
        strings.delete(key);
        assert_eq!(strings.count, 3); // tombstone
        assert!(strings.get(key).is_none());
        strings.add(key);
        assert!(strings.get(key).is_some());
        assert_eq!(strings.count, 3);
    }

    #[test]
    fn add() {
        let mut strings = Strings::new();
        let key = strings.add("a");
        assert_eq!(unsafe { &(*key).data }, "a");
        let a = strings.get("a").unwrap() as *const _;
        let key2 = strings.add("a");
        assert!(std::ptr::eq(key, key2));
    }

    #[test]
    fn cmp() {
        let a = "a".to_string();
        assert_eq!(a, *"a");
    }
}
