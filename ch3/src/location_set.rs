use asm::{Arg, Reg};
use ast::IdxVar;
use indexmap::map::Entry;
use indexmap::IndexMap;
use num_traits::{FromPrimitive, ToPrimitive};
use smallvec::SmallVec;
use std::fmt;
use std::fmt::Write;
use std::ops::Deref;
use support::WritePretty;

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
    if self.0.is_empty() {
      self.0.push(0);
    }
    self.0[i / 32] |= 1 << (i % 32);
  }

  pub fn remove_reg(&mut self, reg: Reg) {
    let i = reg_index(reg);
    if !self.0.is_empty() {
      self.0[i / 32] &= !(1 << (i % 32));
    }
  }

  pub fn add_var(&mut self, var: Var) {
    let i = var_index(var);
    let offset = i / 32;
    if offset >= self.0.len() {
      self.0.resize(offset + 1, 0);
    }
    self.0[offset] |= 1 << (i % 32);
  }

  pub fn remove_var(&mut self, var: Var) {
    let i = var_index(var);
    let offset = i / 32;
    if offset < self.0.len() {
      self.0[offset] &= !(1 << (i % 32));
    }
  }

  pub fn remove_caller_saved_regs(&mut self) {
    self.remove_reg(Reg::Rax);
    self.remove_reg(Reg::Rcx);
    self.remove_reg(Reg::Rdx);
    self.remove_reg(Reg::Rsi);
    self.remove_reg(Reg::Rdi);
    self.remove_reg(Reg::R8);
    self.remove_reg(Reg::R9);
    self.remove_reg(Reg::R10);
    self.remove_reg(Reg::R11);
  }

  pub fn add_argument_regs(&mut self, arity: usize) {
    const ARGUMENT_REGS: [Reg; 6] =
      [Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9];

    assert!(arity <= 6);
    for reg in &ARGUMENT_REGS[..arity] {
      self.add_reg(*reg);
    }
  }

  pub fn write(&self, f: &mut impl Write, var_store: &VarStore) -> fmt::Result {
    let elems = self.0.iter().enumerate().flat_map(|(i, &(mut k))| {
      let base = i * 32;
      let mut elems = vec![];
      while k != 0 {
        let i = base + k.trailing_zeros() as usize;
        elems.push(if i < 16 {
          Arg::Reg(Reg::from_usize(i).unwrap())
        } else {
          Arg::Var(var_store.0.get_index(i - 16).unwrap().0.clone())
        });
        k &= k - 1;
      }
      elems
    });

    write!(f, "{{")?;
    let mut comma = false;
    for elem in elems {
      if comma {
        write!(f, ", ")?;
      }
      comma = true;
      elem.write(f)?;
    }
    write!(f, "}}")
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
      Entry::Vacant(entry) => *entry.insert(Var(len)),
    }
  }
}

impl Deref for VarStore {
  type Target = IndexMap<IdxVar, Var>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
