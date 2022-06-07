use super::interference::Info as OldInfo;
use asm::{Block, Program, Reg};
use ast::{IdxVar, Type};
use ch3::pass::register_allocation::{
  gen_assign_instr_registers, RegisterAlloc,
};
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;
use std::fmt::{self, Formatter, Display};

pub struct Info {
  pub locals: IndexMap<IdxVar, Type>,
  /// in bytes
  pub stack_space: usize,
  pub used_callee_saved_regs: IndexSet<Reg>,
}

impl Display for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    write!(f, "used_callee_saved_regs: ",)?;
    let mut comma = false;
    for reg in &self.used_callee_saved_regs {
      if comma {
        write!(f, ", ")?;
      }
      comma = true;
      write!(f, "{}", reg)?;
    }
    writeln!(f, "\nstack_space: {} bytes", self.stack_space)
  }
}

pub fn allocate_registers(
  prog: Program<OldInfo, IdxVar>,
  available_regs: &[Reg],
) -> Program<Info> {
  assert!(prog.funs.is_empty());

  let (info, blocks) = alloc_body_regs(prog.info, prog.blocks, available_regs);

  Program {
    info,
    blocks,
    funs: vec![],
    ..prog
  }
}

fn alloc_body_regs(
  info: OldInfo,
  blocks: Vec<Block<IdxVar>>,
  available_regs: &[Reg],
) -> (Info, Vec<Block>) {
  let mut num_locals = 0;
  let new_blocks;
  let used_callee_saved_regs;

  {
    let reg_colors: HashMap<_, _> = available_regs
      .iter()
      .enumerate()
      .map(|(i, &reg)| (reg, i as _))
      .collect();

    let mut alloc = RegisterAlloc {
      num_locals: info.locals.len(),
      conflicts: &info.conflicts,
      moves: &info.moves,
      reg_colors: &reg_colors,
      available_regs,
      used_callee_saved_regs: IndexSet::new(),
      assign_instr_registers: gen_assign_instr_registers(
        &info.var_store,
        available_regs,
        &mut num_locals,
      ),
    };

    new_blocks = blocks
      .into_iter()
      .map(|block| alloc.allocate_block_registers(block))
      .collect();

    used_callee_saved_regs = alloc.used_callee_saved_regs;
  }

  (
    Info {
      locals: info.locals,
      stack_space: num_locals * 8,
      used_callee_saved_regs,
    },
    new_blocks,
  )
}
