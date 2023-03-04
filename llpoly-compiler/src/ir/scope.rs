use std::{collections::HashSet, fmt::Display};

use linked_hash_map::LinkedHashMap;

use super::value::ValueId;

/// A scope context is a set of scopes.
#[derive(Debug, Clone)]
pub struct ScopeRegistry {
    pub all_scopes: Vec<ScopeNode>,
    pub all_decls: HashSet<ValueId>,
}
/// A Scope id is a unique identifier for a scope.
/// It also represents the index of the scope in the `ScopeContext.all_scopes`.
#[derive(Debug, Default, Eq, Hash, PartialEq, Clone, Copy)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn root() -> ScopeId {
        ScopeId(0)
    }
}

impl Display for ScopeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<usize> for ScopeId {
    fn from(id: usize) -> ScopeId {
        ScopeId(id)
    }
}
impl From<ScopeId> for usize {
    fn from(id: ScopeId) -> usize {
        id.0
    }
}

#[derive(Debug)]
/// A iterator for walking through the scope tree, in a depth-first manner.
pub struct ScopeCursor<'l_sr> {
    /// The scope context.
    pub scope_ctx: &'l_sr ScopeRegistry,
    /// The path from the root scope to the current scope.
    /// The first element of the tuple is the index of the current child scope.
    pub path: Vec<(usize, ScopeId)>,
}

impl<'lctx> ScopeCursor<'lctx> {
    pub fn new(scope_ctx: &'lctx ScopeRegistry) -> ScopeCursor<'lctx> {
        ScopeCursor {
            scope_ctx,
            path: vec![(0, ScopeId::root())],
        }
    }

    /// Enter the first child scope.
    pub fn enter_child(&mut self) -> Option<ScopeId> {
        // get current scope
        let (_, scope_id) = *self.path.last().unwrap();
        let scope = &self.scope_ctx.all_scopes[usize::from(scope_id)];
        if !scope.children_ids.is_empty() {
            let child_scope_id = scope.children_ids[0];
            log::trace!("enter child scope from {} to {}", scope_id, child_scope_id);
            self.path.push((0, child_scope_id));
            Some(child_scope_id)
        } else {
            None
        }
    }

    /// Enter the next sibling scope.
    pub fn enter_next(&mut self) -> Option<ScopeId> {
        // get parent scope
        assert!(self.path.len() >= 2);
        let (idx, _) = *self.path.last().unwrap();
        let (_, scope_id) = self.path[self.path.len() - 2];
        let scope = &self.scope_ctx.all_scopes[usize::from(scope_id)];
        if idx + 1 < scope.children_ids.len() {
            self.path.pop();
            let sib_scope_id = scope.children_ids[idx + 1];
            log::trace!("enter next scope from {} to {}", scope_id, sib_scope_id);
            self.path.push((idx + 1, sib_scope_id));
            Some(sib_scope_id)
        } else {
            None
        }
    }

    /// Leave the current scope.
    /// Return the parent scope id.
    pub fn leave(&mut self) -> Option<ScopeId> {
        if self.path.len() > 1 {
            let scope_id = self.path.last().unwrap().1;
            self.path.pop();
            let (_, parent_scope_id) = *self.path.last().unwrap();
            log::trace!("leave scope from {} to {}", scope_id, parent_scope_id);
            Some(parent_scope_id)
        } else {
            None
        }
    }

    pub fn scope(&self) -> &ScopeNode {
        let (_, scope_id) = *self.path.last().unwrap();
        &self.scope_ctx.all_scopes[usize::from(scope_id)]
    }

    pub fn path_string(&self) -> String {
        let mut path = String::new();
        for (idx, scope_id) in &self.path {
            let scope = &self.scope_ctx.all_scopes[usize::from(*scope_id)];
            if idx == &0 {
                path.push_str(&format!("{}:{}", scope_id, scope.sym_map.len()));
            } else {
                path.push_str(&format!(">{}:{}", scope_id, scope.sym_map.len()));
            }
        }
        path
    }
}

impl Default for ScopeRegistry {
    /// Create a new scope context with a global scope.
    fn default() -> Self {
        let mut all_scopes = Vec::new();
        let all_decls = HashSet::new();
        let global_scope = ScopeNode {
            id: ScopeId::root(),
            children_ids: Vec::new(),
            sym_map: LinkedHashMap::new(),
            parent_id: ScopeId::root(),
        };
        all_scopes.push(global_scope);
        ScopeRegistry {
            all_scopes,
            all_decls,
        }
    }
}

impl ScopeRegistry {
    /// Create a new sub scope.
    pub fn derive(&mut self, parent: ScopeId) -> ScopeId {
        let new_scope_id = self.all_scopes.len().into();
        let new_scope = ScopeNode {
            id: new_scope_id,
            children_ids: Vec::new(),
            sym_map: LinkedHashMap::new(),
            parent_id: parent,
        };
        self.all_scopes.push(new_scope);
        self.all_scopes[usize::from(parent)]
            .children_ids
            .push(new_scope_id);
        new_scope_id
    }

    /// Enter the parent scope of the current.
    pub fn leave(&mut self, child: ScopeId) -> ScopeId {
        self.all_scopes[usize::from(child)].parent_id
    }

    /// Register a new symbol in the current scope.
    pub fn register(
        &mut self,
        scope: ScopeId,
        symbol_name: String,
        symbol_id: ValueId,
    ) -> Option<(ScopeId, ValueId)> {
        let scope = &mut self.all_scopes[usize::from(scope)];
        if let Some(old_symbol_id) = scope.sym_map.insert(symbol_name, symbol_id) {
            return Some((scope.id, old_symbol_id));
        }
        self.all_decls.insert(symbol_id);
        None
    }

    /// Resolve a symbol name to a symbol id.
    pub fn resolve(&self, scope_id: ScopeId, symbol_name: &str) -> Option<ValueId> {
        let mut scope_id = scope_id;
        loop {
            let scope = &self.all_scopes[usize::from(scope_id)];
            if let Some(symbol_id) = scope.sym_map.get(symbol_name) {
                return Some(*symbol_id);
            }
            if scope.id == ScopeId::root() {
                return None;
            }
            scope_id = scope.parent_id;
        }
    }
}
#[derive(Debug, Clone)]
/// A scope node is a node in the scope tree.
pub struct ScopeNode {
    pub id: ScopeId,
    pub children_ids: Vec<ScopeId>,
    pub parent_id: ScopeId,

    pub sym_map: LinkedHashMap<String, ValueId>,
}

impl ScopeNode {
    pub fn new(id: ScopeId, parent_id: ScopeId) -> ScopeNode {
        ScopeNode {
            id,
            children_ids: Vec::new(),
            parent_id,
            sym_map: LinkedHashMap::new(),
        }
    }
}
