use asm::Reg;
use ast::IdxVar;
use num_traits::ToPrimitive;
use smallvec::SmallVec;
use indexmap::IndexMap;
use indexmap::map::Entry;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct LocationSet(SmallVec<[u32; 2]>);

#[derive(Debug, Clone)]
pub struct VarStore(IndexMap<IdxVar, Var>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Var(pub usize);

impl LocationSet {
  pub fn new() -> Self {
    Self(SmallVec::new())
  }

  pub fn add_reg(&mut self, reg: Reg) {
    let i = reg_index(reg);
    self.0[i / 32] |= 1 << (i % 32);
  }

  pub fn remove_reg(&mut self, reg: Reg) {
    let i = reg_index(reg);
    self.0[i / 32] &= !(1 << (i % 32));
  }

  pub fn add_var(&mut self, var: Var) {
    let i = var_index(var);
    self.0[i / 32] |= 1 << (i % 32);
  }

  pub fn remove_var(&mut self, var: Var) {
    let i = var_index(var);
    self.0[i / 32] &= !(1 << (i % 32));
  }
}

fn reg_index(reg: Reg) -> usize {
  reg.to_usize().unwrap()
}

fn var_index(var: Var) -> usize {
  var.0 + 16
}

impl VarStore {
  pub fn new() -> Self {
    Self(IndexMap::new())
  }

  pub fn get(&mut self, var: IdxVar) -> Var {
    let len = self.0.len();
    match self.0.entry(var) {
      Entry::Occupied(var) => *var.get(),
      Entry::Vacant(entry) => {
        *entry.insert(Var(len))
      }
    }
  }
}

impl Deref for VarStore {
  type Target = IndexMap<IdxVar, Var>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}