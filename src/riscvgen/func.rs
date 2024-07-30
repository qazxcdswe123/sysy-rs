use std::{cell::Cell, collections::HashMap};

use koopa::ir::{entities::ValueData, BasicBlock, Function};

/// A stack slot.
#[derive(Clone, Copy)]
pub struct Slot {
    pub offset: usize,
    /// `true` if the slot stores an pointer but not an allocation.
    pub is_ptr: bool,
}

impl Slot {
    fn new(offset: usize, is_ptr: bool) -> Self {
        Self { offset, is_ptr }
    }

    /// Maps the offset of the slot.
    fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(usize) -> usize,
    {
        Self {
            offset: f(self.offset),
            is_ptr: self.is_ptr,
        }
    }
}

pub struct FunctionContext {
    func: Function,
    /// None if the curr function is a leaf function.
    max_arg_num: Option<usize>,
    alloc_size: usize,
    allocs: HashMap<*const ValueData, Slot>,
    bbs: HashMap<BasicBlock, String>,
    sp_offset: Cell<Option<usize>>,
}

impl FunctionContext {
    thread_local! {
        static NEXT_TEMP_LABEL_ID: Cell<usize> = Cell::new(0);
    }

    pub fn new(func: Function) -> Self {
        Self {
            func,
            max_arg_num: None,
            alloc_size: 0,
            allocs: HashMap::new(),
            bbs: HashMap::new(),
            sp_offset: Cell::new(None),
        }
    }

    pub fn func(&self) -> Function {
        self.func
    }

    pub fn log_arg_num(&mut self, arg_num: usize) {
        if self.max_arg_num.is_none() || self.max_arg_num.unwrap() < arg_num {
            self.max_arg_num = Some(arg_num);
        }
    }

    pub fn is_leaf(&self) -> bool {
        self.max_arg_num.is_none()
    }

    pub fn sp_offset(&self) -> usize {
        if let Some(offset) = self.sp_offset.get() {
            offset
        } else {
            let ra = if self.is_leaf() { 0 } else { 4 };
            let args = match self.max_arg_num {
                Some(n) if n > 8 => (n - 8) * 4,
                _ => 0,
            };
            let offset = ra + self.alloc_size + args;
            // align to 16
            let sp_offset = (offset + 15) & 0xF;
            self.sp_offset.set(Some(sp_offset));
            sp_offset
        }
    }

    pub fn alloc_slot(&mut self, value: &ValueData) {
        match value.kind() {
            koopa::ir::ValueKind::Alloc(_) => {
                self.allocs.insert(value, Slot::new(self.alloc_size, false));
                self.alloc_size += match value.ty().kind() {
                    koopa::ir::TypeKind::Pointer(p) => p.size(),
                    _ => unreachable!(),
                }
            }
            _ => {
                let is_ptr = matches!(value.ty().kind(), koopa::ir::TypeKind::Pointer(_));
                self.allocs
                    .insert(value, Slot::new(self.alloc_size, is_ptr));
                self.alloc_size += value.ty().size();
            }
        }
    }

    /// Returns the offset of the stack pointer.
    /// TODO: figure out
    pub fn slot_offset(&self, value: &ValueData) -> Option<Slot> {
        self.allocs
            .get(&(value as *const ValueData))
            .map(|&offset| {
                if self.is_leaf() {
                    offset.map(|o| self.sp_offset() - self.alloc_size + o)
                } else {
                    offset.map(|o| o)
                }
            })
    }

    pub fn bb_name(&self, bb: BasicBlock) -> &str {
        self.bbs.get(&bb).as_ref().unwrap()
    }

    pub fn log_bb_name(&mut self, bb: BasicBlock, name: &Option<String>) {
        let id = Self::NEXT_TEMP_LABEL_ID.with(|id| id.replace(id.get() + 1));
        let name = match name.as_ref() {
            Some(name) => format!(".L{}_{}", &name[1..], id),
            None => format!(".L{}", id),
        };
        self.bbs.insert(bb, name);
    }
}
