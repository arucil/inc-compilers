use asm::{Arg, Reg};
use ast::IdxVar;
use indexmap::map::Entry;
use indexmap::IndexMap;
use num_traits::{FromPrimitive, ToPrimitive};
use smallvec::SmallVec;
use std::fmt::{self, Debug, Write};
use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct LocationSet(SmallVec<[u32; 2]>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location(usize);

#[derive(Debug, Clone)]
pub struct VarStore(IndexMap<IdxVar, Var>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(usize);

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

  pub fn iter(&self) -> LocationIter {
    let current = if self.0.len() > 0 { self.0[0] } else { 0 };
    LocationIter {
      set: self,
      index: 0,
      current,
    }
  }

  pub fn write(&self, f: &mut impl Write, var_store: &VarStore) -> fmt::Result {
    write!(f, "{{")?;
    let mut comma = false;
    for elem in self {
      if comma {
        write!(f, ", ")?;
      }
      comma = true;
      write!(f, "{:?}", elem.to_arg(var_store))?;
    }
    write!(f, "}}")
  }
}

pub struct LocationIter<'a> {
  set: &'a LocationSet,
  index: usize,
  current: u32,
}

impl<'a> Iterator for LocationIter<'a> {
  type Item = Location;

  fn next(&mut self) -> Option<Location> {
    while self.current == 0 {
      self.index += 1;
      if self.index >= self.set.0.len() {
        return None;
      }
      self.current = self.set.0[self.index];
    }
    let i = self.current.trailing_zeros() as usize + self.index * 32;
    self.current &= self.current - 1;
    Some(Location(i))
  }
}

impl<'a> IntoIterator for &'a LocationSet {
  type Item = Location;
  type IntoIter = LocationIter<'a>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
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

impl Var {
  pub fn to_arg(&self, var_store: &VarStore) -> Arg<IdxVar> {
    Arg::Var(var_store.get_index(self.0 - 16).unwrap().0.clone())
  }
}

impl Location {
  pub fn to_arg(&self, var_store: &VarStore) -> Arg<IdxVar> {
    if self.0 < 16 {
      Arg::Reg(Reg::from_usize(self.0).unwrap())
    } else {
      Arg::Var(var_store.0.get_index(self.0 - 16).unwrap().0.clone())
    }
  }

  pub fn from_arg(arg: Arg<IdxVar>, var_store: &mut VarStore) -> Option<Self> {
    match arg {
      Arg::Deref(reg, _) | Arg::Reg(reg) => Some(reg.into()),
      Arg::Var(var) => Some(Self(var_store.get(var).0 + 16)),
      Arg::Imm(_) => None,
      Arg::ByteReg(_) => unimplemented!(),
    }
  }

  pub fn to_reg(&self) -> Option<Reg> {
    Reg::from_usize(self.0)
  }

  pub fn to_var(&self) -> Option<Var> {
    if self.0 >= 16 {
      Some(Var(self.0 - 16))
    } else {
      None
    }
  }
}

impl From<Reg> for Location {
  fn from(reg: Reg) -> Self {
    Self(reg.to_usize().unwrap())
  }
}
