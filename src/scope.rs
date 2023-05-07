use id_arena::{Arena, Id};
use std::collections::HashMap;

use crate::symbol::Symbol;

pub struct Scope {
    symbols: HashMap<String, SymbolId>,
    parent: Option<ScopeId>,
    children: Vec<ScopeId>,
}

type SymbolArena = Arena<Symbol>;
type ScopeArena = Arena<Scope>;
pub type SymbolId = Id<Symbol>;
pub type ScopeId = Id<Scope>;

impl Scope {
    // 创建一个新的作用域
    pub fn new(parent: Option<ScopeId>) -> Self {
        Scope {
            symbols: HashMap::new(),
            parent,
            children: Vec::new(),
        }
    }

    // 将符号插入当前作用域
    pub fn insert(&mut self, name: String, symbol_id: SymbolId) -> Option<SymbolId> {
        self.symbols.insert(name, symbol_id)
    }

    // 查询符号，从当前作用域开始查找，如果找不到，递归查找父作用域
    pub fn lookup<'a>(&self, name: &str, scope_arena: &'a ScopeArena) -> Option<SymbolId> {
        if let Some(symbol_id) = self.symbols.get(name) {
            Some(*symbol_id)
        } else if let Some(parent_id) = self.parent {
            let parent = &scope_arena[parent_id];
            parent.lookup(name, scope_arena)
        } else {
            None
        }
    }
}

pub struct SymbolTable {
    symbol_arena: SymbolArena,
    scope_arena: ScopeArena,
    current_scope: ScopeId,
}

impl SymbolTable {
    // 创建一个新的符号表
    pub fn new() -> Self {
        let mut scope_arena = ScopeArena::new();
        let root_scope = scope_arena.alloc(Scope::new(None));
        SymbolTable {
            symbol_arena: SymbolArena::new(),
            scope_arena,
            current_scope: root_scope,
        }
    }

    // 进入一个新的作用域
    pub fn enter_scope(&mut self) {
        let new_scope = self.scope_arena.alloc(Scope::new(Some(self.current_scope)));
        self.scope_arena[self.current_scope]
            .children
            .push(new_scope);
        self.current_scope = new_scope;
    }

    // 离开当前作用域，返回到父作用域
    pub fn leave_scope(&mut self) {
        if let Some(parent_id) = self.scope_arena[self.current_scope].parent {
            self.current_scope = parent_id;
        }
    }

    // 在当前作用域中插入一个符号
    pub fn insert_symbol(&mut self, name: String, symbol: Symbol) -> SymbolId {
        let symbol_id = self.symbol_arena.alloc(symbol);
        self.scope_arena[self.current_scope].insert(name, symbol_id);
        symbol_id
    }

    // 查找一个符号
    pub fn lookup_symbol(&self, name: &str) -> Option<SymbolId> {
        self.scope_arena[self.current_scope].lookup(name, &self.scope_arena)
    }

    // 打印符号表，并返回字符串
    pub fn print_table(&self) -> String {
        self.print_scope(self.current_scope, 0)
    }

    pub fn scope_id(&self) -> ScopeId {
        self.current_scope
    }

    // 打印作用域，并返回字符串
    fn print_scope(&self, scope_id: ScopeId, level: usize) -> String {
        let mut result = String::new();
        let scope = &self.scope_arena[scope_id];
        for (name, symbol_id) in &scope.symbols {
            let symbol = &self.symbol_arena[*symbol_id];
            result += &format!("{:indent$}{}: {:?}\n", "", name, symbol, indent = level * 4);
        }
        for child_id in &scope.children {
            result += &self.print_scope(*child_id, level + 1);
        }
        result
    }
}

use std::fmt;

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Symbols: {:?}", self.symbols)?;
        writeln!(f, "Parent: {:?}", self.parent)?;
        writeln!(f, "Children: {:?}", self.children)
    }
}

impl fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Symbol Arena: {:?}", self.symbol_arena)?;
        writeln!(f, "Scope Arena: {:?}", self.scope_arena)?;
        writeln!(f, "Current Scope: {:?}", self.current_scope)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table() {
        let mut symbol_table = SymbolTable::new();

        // 在根作用域插入符号a
        let symbol_a = symbol_table.insert_symbol("a".to_string(), Symbol::Dummy);

        // 查询符号a
        let found_a = symbol_table.lookup_symbol("a");
        assert_eq!(found_a, Some(symbol_a));

        // 查询不存在的符号b
        let not_found_b = symbol_table.lookup_symbol("b");
        assert_eq!(not_found_b, None);

        // 进入一个新的作用域
        symbol_table.enter_scope();

        // 在子作用域插入符号b
        let symbol_b = symbol_table.insert_symbol("b".to_string(), Symbol::Dummy);

        // 查询符号a（从父作用域继承）
        let found_a_in_child = symbol_table.lookup_symbol("a");
        assert_eq!(found_a_in_child, Some(symbol_a));

        // 查询符号b
        let found_b = symbol_table.lookup_symbol("b");
        assert_eq!(found_b, Some(symbol_b));

        // 离开子作用域
        symbol_table.leave_scope();

        // 在根作用域查询符号b，应该找不到
        let not_found_b_in_root = symbol_table.lookup_symbol("b");
        assert_eq!(not_found_b_in_root, None);
    }

    #[test]
    fn test_symbol_table2() {
        let mut symbol_table = SymbolTable::new();

        // 在根作用域插入符号a
        let _ = symbol_table.insert_symbol("a".to_string(), Symbol::Dummy);

        // 进入一个新的作用域
        symbol_table.enter_scope();

        // 在子作用域插入符号b
        let _ = symbol_table.insert_symbol("b".to_string(), Symbol::Dummy);

        // 在子作用域插入符号c
        let _ = symbol_table.insert_symbol("c".to_string(), Symbol::Dummy);

        // 进入一个新的作用域
        symbol_table.enter_scope();

        // 在子子作用域插入符号d
        let _ = symbol_table.insert_symbol("d".to_string(), Symbol::Dummy);

        // 在子子作用域插入符号e
        let _ = symbol_table.insert_symbol("e".to_string(), Symbol::Dummy);

        // 打印符号表
        symbol_table.print_table();

        // 离开子子作用域
        symbol_table.leave_scope();

        // 打印符号表
        symbol_table.print_table();

        // 离开子作用域
        symbol_table.leave_scope();

        // 打印符号表
        symbol_table.print_table();
    }
}
