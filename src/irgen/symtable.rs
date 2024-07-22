use std::{collections::HashMap, hash::Hash};
use koopa::ir::Value;

pub enum Symbol {
    Const(i32),
    Var(Value),
}

pub struct SymTable {
    table: ChainMap<String, Symbol>,
}

struct ChainMap<K: Eq + Hash, V> {
    maps: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash, V> ChainMap<K, V> {
    fn new() -> Self {
        Self { maps: Vec::new() }
    }

    fn insert(&mut self, key: K, value: V) {
        if let Some(map) = self.maps.last_mut() {
            map.insert(key, value);
        } else {
            let mut map = HashMap::new();
            map.insert(key, value);
            self.maps.push(map);
        }
    }

    fn get(&self, key: &K) -> Option<&V> {
        for map in self.maps.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some(value);
            }
        }
        None
    }

    fn push(&mut self) {
        self.maps.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.maps.pop();
    }
}

impl SymTable {
    pub fn new() -> Self {
        let mut table = ChainMap::new();
        table.push();
        Self { table }
    }

    pub fn insert(&mut self, name: String, symbol: Symbol) {
        self.table.insert(name, symbol);
    }

    pub fn get(&self, name: &String) -> Option<&Symbol> {
        self.table.get(name)
    }
}