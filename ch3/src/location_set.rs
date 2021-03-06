use asm::{Arg, Reg};
use ast::IdxVar;
use indexmap::IndexMap;
use num_traits::{FromPrimitive, ToPrimitive};
use smallvec::SmallVec;
use std::fmt::{self, Debug, Write};
use std::ops::{BitOrAssign, Deref};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocationSet(SmallVec<[u32; 2]>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location(usize);

#[derive(Debug, Clone, Default)]
pub struct VarStore(IndexMap<IdxVar, Var>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(usize);

impl Default for LocationSet {
  fn default() -> Self {
    Self::new()
  }
}

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

  pub fn regs<const N: usize>(regs: [Reg; N]) -> Self {
    let mut new = Self::new();
    for reg in regs {
      new.add_reg(reg);
    }
    new
  }

  pub fn remove_reg(&mut self, reg: Reg) {
    let i = reg_index(reg);
    if !self.0.is_empty() {
      let ix = i / 32;
      self.0[ix] &= !(1 << (i % 32));
      if ix == self.0.len() - 1 {
        while let Some(&0) = self.0.last() {
          self.0.pop();
        }
      }
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

  pub fn iter(&self) -> LocationIter {
    let current = if self.0.is_empty() { 0 } else { self.0[0] };
    LocationIter {
      set: self,
      index: 0,
      current,
    }
  }

  pub fn clear(&mut self) {
    self.0.fill(0);
  }

  pub fn write(&self, f: &mut impl Write, var_store: &VarStore) -> fmt::Result {
    write!(f, "{{")?;
    let mut comma = false;
    for elem in self {
      if comma {
        write!(f, ", ")?;
      }
      comma = true;
      write!(f, "{}", elem.to_arg(var_store))?;
    }
    write!(f, "}}")
  }
}

impl<'a> BitOrAssign<&'a Self> for LocationSet {
  fn bitor_assign(&mut self, rhs: &'a LocationSet) {
    if self.0.len() < rhs.0.len() {
      self.0.resize(rhs.0.len(), 0);
    }
    for i in 0..self.0.len().min(rhs.0.len()) {
      self.0[i] |= rhs.0[i];
    }
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
    Self::default()
  }

  pub fn insert(&mut self, var: IdxVar) -> Var {
    let len = self.0.len();
    *self.0.entry(var).or_insert(Var(len))
  }

  pub fn get(&self, var: IdxVar) -> Var {
    self.0[&var]
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

  pub fn to_arg_var(&self, var_store: &VarStore) -> Option<IdxVar> {
    if self.0 < 16 {
      None
    } else {
      Some(var_store.0.get_index(self.0 - 16).unwrap().0.clone())
    }
  }

  pub fn from_arg(arg: Arg<IdxVar>, var_store: &VarStore) -> Option<Self> {
    match arg {
      Arg::Deref(reg, _) | Arg::Reg(reg) => Some(reg.into()),
      Arg::Var(var) => Some(Self(var_store.get(var).0 + 16)),
      Arg::Imm(_) => None,
      Arg::ByteReg(reg) => Some(Reg::from(reg).into()),
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

impl From<Var> for Location {
  fn from(var: Var) -> Self {
    Self(var.0 + 16)
  }
}
