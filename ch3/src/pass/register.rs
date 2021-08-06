use super::interference::{Info as OldInfo, InterferenceGraph};
use crate::location_set::{Location, Var, VarStore};
use asm::{Arg, Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::{IndexMap, IndexSet};
use num_traits::ToPrimitive;
use petgraph::graph::NodeIndex;
use priority_queue::PriorityQueue;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  /// in bytes
  pub stack_space: usize,
  pub used_callee_saved_regs: IndexSet<Reg>,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}", self.locals)?;
    writeln!(f, "used_callee_saved_regs: {:?}", self.used_callee_saved_regs)?;
    writeln!(f, "stack_space: {} bytes", self.stack_space)
  }
}

pub fn allocate_registers(
  prog: Program<OldInfo, IdxVar>,
  available_regs: &[Reg],
) -> Program<Info> {
  let reg_colors: HashMap<_, _> = available_regs
    .iter()
    .enumerate()
    .map(|(i, &reg)| (reg, i as i32))
    .collect();
  let mut var_store = prog.info.var_store;
  let conflicts = prog.info.conflicts;
  let mut max_locals = 0;
  let mut used_callee_saved_regs = IndexSet::new();

  let blocks = prog
    .blocks
    .into_iter()
    .map(|(label, block)| {
      let conflicts = &conflicts[&label];
      let (block, locals) = allocate_registers_block(
        block,
        &mut var_store,
        conflicts,
        &reg_colors,
        available_regs,
        &mut used_callee_saved_regs,
      );
      if locals > max_locals {
        max_locals = locals;
      }
      (label, block)
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: max_locals * 8,
      used_callee_saved_regs,
    },
    blocks,
  }
}

fn allocate_registers_block(
  block: Block<IdxVar>,
  var_store: &mut VarStore,
  conflicts: &InterferenceGraph,
  reg_colors: &HashMap<Reg, i32>,
  available_regs: &[Reg],
  used_callee_saved_regs: &mut IndexSet<Reg>,
) -> (Block, usize) {
  let var_colors = color_graph(conflicts, reg_colors);
  let mut max_locals = 0;

  for (_, c) in &var_colors {
    if let Some(&reg) = available_regs.get(c.0 as usize) {
      if reg.is_callee_saved() {
        used_callee_saved_regs.insert(reg);
      }
    }
  }

  let block = Block {
    code: block
      .code
      .into_iter()
      .map(|instr| {
        let (instr, locals) =
          assign_instr_var(instr, var_store, &var_colors, available_regs);
        if locals > max_locals {
          max_locals = locals;
        }
        instr
      })
      .collect(),
  };
  (block, max_locals)
}

fn assign_instr_var(
  instr: Instr<IdxVar>,
  var_store: &mut VarStore,
  var_colors: &IndexMap<Var, Color>,
  available_regs: &[Reg],
) -> (Instr, usize) {
  let mut max_locals = 0;
  let mut assign = |arg| match arg {
    Arg::Deref(reg, i) => Arg::Deref(reg, i),
    Arg::Reg(reg) => Arg::Reg(reg),
    Arg::Imm(i) => Arg::Imm(i),
    Arg::Var(var) => {
      let i = var_colors[&var_store.get(var)].0 as usize;
      if let Some(&reg) = available_regs.get(i) {
        Arg::Reg(reg)
      } else {
        let local = i - available_regs.len() + 1;
        if local + 1 > max_locals {
          max_locals = local;
        }
        Arg::Deref(Reg::Rbp, -8 * local as i32)
      }
    }
  };

  let instr = match instr {
    Instr::Add(src, dest) => Instr::Add(assign(src), assign(dest)),
    Instr::Call(label, arity) => Instr::Call(label, arity),
    Instr::Jmp(label) => Instr::Jmp(label),
    Instr::Mov(src, dest) => Instr::Mov(assign(src), assign(dest)),
    Instr::Neg(dest) => Instr::Neg(assign(dest)),
    Instr::Pop(dest) => Instr::Pop(assign(dest)),
    Instr::Push(src) => Instr::Push(assign(src)),
    Instr::Ret => Instr::Ret,
    Instr::Sub(src, dest) => Instr::Sub(assign(src), assign(dest)),
    Instr::Syscall => Instr::Syscall,
    _ => unreachable!("{:?}", instr),
  };
  (instr, max_locals)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Color(i32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct NodeState {
  location: Location,
  color: Option<Color>,
  saturation: BTreeSet<i32>,
}

impl NodeState {
  fn new(location: Location) -> Self {
    Self {
      location,
      color: None,
      saturation: BTreeSet::new(),
    }
  }
}

impl PartialOrd for NodeState {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.saturation.len().partial_cmp(&other.saturation.len())
  }
}

impl Ord for NodeState {
  fn cmp(&self, other: &Self) -> Ordering {
    self.saturation.len().cmp(&other.saturation.len())
  }
}

fn color_graph(
  graph: &InterferenceGraph,
  reg_colors: &HashMap<Reg, i32>,
) -> IndexMap<Var, Color> {
  let mut queue = PriorityQueue::<NodeIndex, NodeState>::new();

  for node in graph.graph.node_indices() {
    let location = graph.graph[node];
    queue.push(node, NodeState::new(location));
  }

  for node in graph.graph.node_indices() {
    if let Some(reg) = graph.graph[node].to_reg() {
      let color = reg_colors
        .get(&reg)
        .map_or_else(|| !reg.to_i32().unwrap(), |&c| c);
      for neighbor in graph.graph.neighbors(node) {
        queue.change_priority_by(&neighbor, |v| {
          v.saturation.insert(color);
        })
      }
      queue.remove(&node);
    }
  }

  let mut var_colors = IndexMap::new();

  while let Some((node, state)) = queue.pop() {
    let mut last_color_ix = -1;
    for ix in state.saturation {
      if ix < 0 {
        continue;
      }
      if ix == last_color_ix + 1 {
        last_color_ix = ix;
      } else {
        break;
      }
    }
    let color = Color(last_color_ix + 1);
    for neighbor in graph.graph.neighbors(node) {
      queue.change_priority_by(&neighbor, |v| {
        v.saturation.insert(color.0);
      })
    }
    let var = state.location.to_var().unwrap();
    var_colors.insert(var, color);
  }

  var_colors
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::{Block, Arg};
  use ch2::pass::instruction::Info as OldOldInfo;
  use insta::assert_snapshot;
  use crate::location_set::LocationSet;
  use maplit::hashmap;

  fn var(name: &str) -> Arg<IdxVar> {
    Arg::Var(IdxVar {
      name: name.to_owned(),
      index: 0,
    })
  }

  #[test]
  fn example_in_book() {
    use asm::Reg::*;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Mov(Imm(1), var("v")),
      Mov(Imm(42), var("w")),
      Mov(var("v"), var("x")),
      Add(Imm(7), var("x")),
      Mov(var("x"), var("y")),
      Mov(var("x"), var("z")),
      Add(var("w"), var("z")),
      Mov(var("y"), var("t")),
      Neg(var("t")),
      Mov(var("z"), Reg(Rax)),
      Add(var("t"), Reg(Rax)),
      Jmp("conclusion".to_owned()),
    ];
    let label_live = hashmap! {
      "conclusion".to_owned() => {
        let mut set = LocationSet::new();
        set.add_reg(Rax);
        set.add_reg(Rsp);
        set
      }
    };
    let prog = Program {
      info: OldOldInfo {
        locals: IndexSet::new(),
      },
      blocks: vec![("start".to_owned(), Block { code })],
    };
    let prog = super::super::liveness::analyze_liveness(prog, label_live);
    let prog = super::super::interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rcx]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn call() {
    use asm::Reg::*;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Pop(Reg(Rdi)),
      Pop(Reg(Rsi)),
      Push(var("x")),
      Mov(Reg(Rbx), var("w")),
      Call("foo".to_owned(), 3),
      Add(Reg(Rax), var("w")),
      Jmp("conclusion".to_owned()),
    ];
    let label_live = hashmap! {
      "conclusion".to_owned() => {
        let mut set = LocationSet::new();
        set.add_reg(Rax);
        set.add_reg(Rsp);
        set
      }
    };
    let prog = Program {
      info: OldOldInfo {
        locals: IndexSet::new(),
      },
      blocks: vec![("start".to_owned(), Block { code })],
    };
    let prog = super::super::liveness::analyze_liveness(prog, label_live);
    let prog = super::super::interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8, R12]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn call_no_enough_registers() {
    use asm::Reg::*;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Pop(Reg(Rdi)),
      Pop(Reg(Rsi)),
      Push(var("x")),
      Mov(Reg(Rbx), var("w")),
      Call("foo".to_owned(), 3),
      Add(Reg(Rax), var("w")),
      Jmp("conclusion".to_owned()),
    ];
    let label_live = hashmap! {
      "conclusion".to_owned() => {
        let mut set = LocationSet::new();
        set.add_reg(Rax);
        set.add_reg(Rsp);
        set
      }
    };
    let prog = Program {
      info: OldOldInfo {
        locals: IndexSet::new(),
      },
      blocks: vec![("start".to_owned(), Block { code })],
    };
    let prog = super::super::liveness::analyze_liveness(prog, label_live);
    let prog = super::super::interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8]);

    assert_snapshot!(result.to_string_pretty());
  }
}