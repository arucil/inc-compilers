use super::interference::Info as OldInfo;
use asm::{Program, Reg};
use ast::{IdxVar, Type};
use ch3::pass::register_allocation::{
  gen_assign_instr_registers, RegisterAlloc,
};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexMap<IdxVar, Type>,
  /// in bytes
  pub stack_space: usize,
  pub used_callee_saved_regs: IndexSet<Reg>,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    writeln!(
      f,
      "used_callee_saved_regs: {:?}",
      self.used_callee_saved_regs
    )?;
    writeln!(f, "stack_space: {} bytes", self.stack_space)
  }
}

pub fn allocate_registers(
  prog: Program<OldInfo, IdxVar>,
  available_regs: &[Reg],
) -> Program<Info> {
  let mut num_locals = 0;
  let blocks;
  let used_callee_saved_regs;

  {
    let reg_colors: HashMap<_, _> = available_regs
      .iter()
      .enumerate()
      .map(|(i, &reg)| (reg, i as _))
      .collect();

    let mut alloc = RegisterAlloc {
      num_locals: prog.info.locals.len(),
      conflicts: &prog.info.conflicts,
      moves: &prog.info.moves,
      reg_colors: &reg_colors,
      available_regs,
      used_callee_saved_regs: IndexSet::new(),
      assign_instr_registers: gen_assign_instr_registers(
        &prog.info.var_store,
        available_regs,
        &mut num_locals,
      ),
    };

    blocks = prog
      .blocks
      .into_iter()
      .map(|(label, block)| {
        let block = alloc.allocate_block_registers(block);
        (label, block)
      })
      .collect();

    used_callee_saved_regs = alloc.used_callee_saved_regs;
  }

  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: num_locals * 8,
      used_callee_saved_regs,
    },
    constants: prog.constants,
    externs: prog.externs,
    blocks,
  }
}
