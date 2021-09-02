use super::interference::Info as OldInfo;
use crate::location_graph::LocationGraph;
use crate::location_set::{Location, Var, VarStore};
use asm::{Arg, Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::{IndexMap, IndexSet};
use num_traits::ToPrimitive;
use petgraph::graph::NodeIndex;
use priority_queue::PriorityQueue;
use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap, HashSet};
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
  let reg_colors: HashMap<_, _> = available_regs
    .iter()
    .enumerate()
    .map(|(i, &reg)| (reg, i as i32))
    .collect();
  let mut var_store = prog.info.var_store;
  let conflicts = prog.info.conflicts;
  let moves = prog.info.moves;
  let locals: Vec<_> = prog
    .info
    .locals
    .iter()
    .map(|var| var_store.get(var.clone()))
    .collect();
  let mut max_locals = 0;
  let mut used_callee_saved_regs = IndexSet::new();
  let tmp_moves = LocationGraph::new();

  let blocks = prog
    .blocks
    .into_iter()
    .map(|(label, block)| {
      let conflicts = &conflicts[&label];
      let moves = moves.get(&label).unwrap_or(&tmp_moves);
      let (block, locals) = allocate_registers_block(
        block,
        &locals,
        &mut var_store,
        conflicts,
        moves,
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
  locals: &[Var],
  var_store: &mut VarStore,
  conflicts: &LocationGraph,
  moves: &LocationGraph,
  reg_colors: &HashMap<Reg, i32>,
  available_regs: &[Reg],
  used_callee_saved_regs: &mut IndexSet<Reg>,
) -> (Block, usize) {
  let var_colors = color_graph(conflicts, moves, locals, reg_colors);

  for (_, c) in &var_colors {
    if let Some(&reg) = available_regs.get(c.0 as usize) {
      if reg.is_callee_saved() {
        used_callee_saved_regs.insert(reg);
      }
    }
  }

  let block = Block {
    global: block.global,
    code: block
      .code
      .into_iter()
      .map(|instr| {
        assign_instr_var(instr, var_store, &var_colors, available_regs)
      })
      .collect(),
  };

  let num_locals = var_colors
    .values()
    .filter(|c| c.0 as usize >= reg_colors.len())
    .collect::<HashSet<_>>()
    .len();

  (block, num_locals)
}

fn assign_instr_var(
  instr: Instr<IdxVar>,
  var_store: &mut VarStore,
  var_colors: &IndexMap<Var, Color>,
  available_regs: &[Reg],
) -> Instr {
  let mut assign = |arg: Arg<IdxVar>| match arg {
    Arg::Deref(reg, i) => Arg::Deref(reg, i),
    Arg::Reg(reg) => Arg::Reg(reg),
    Arg::Imm(i) => Arg::Imm(i),
    Arg::Var(var) => {
      let i = var_colors[&var_store.get(var)].0 as usize;
      if let Some(&reg) = available_regs.get(i) {
        Arg::Reg(reg)
      } else {
        let local = i - available_regs.len() + 1;
        Arg::Deref(Reg::Rbp, -8 * local as i32)
      }
    }
    Arg::ByteReg(_) => unimplemented!(),
  };

  let instr = match instr {
    Instr::Add { src, dest } => Instr::Add {
      src: assign(src),
      dest: assign(dest),
    },
    Instr::Call(label, arity) => Instr::Call(label, arity),
    Instr::Jmp(label) => Instr::Jmp(label),
    Instr::Mov { src, dest } => Instr::Mov {
      src: assign(src),
      dest: assign(dest),
    },
    Instr::Neg(dest) => Instr::Neg(assign(dest)),
    Instr::Pop(dest) => Instr::Pop(assign(dest)),
    Instr::Push(src) => Instr::Push(assign(src)),
    Instr::Ret => Instr::Ret,
    Instr::Sub { src, dest } => Instr::Sub {
      src: assign(src),
      dest: assign(dest),
    },
    Instr::Syscall => Instr::Syscall,
    _ => unreachable!("{:?}", instr),
  };
  instr
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Color(i32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct NodeState {
  location: Location,
  color: Option<Color>,
  saturation: BTreeSet<i32>,
  num_moves: usize,
}

impl NodeState {
  fn new(location: Location, num_moves: usize) -> Self {
    Self {
      location,
      color: None,
      saturation: BTreeSet::new(),
      num_moves,
    }
  }
}

impl PartialOrd for NodeState {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self.saturation.len().partial_cmp(&other.saturation.len()) {
      Some(Ordering::Equal) => self.num_moves.partial_cmp(&other.num_moves),
      x => x,
    }
  }
}

impl Ord for NodeState {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.saturation.len().cmp(&other.saturation.len()) {
      Ordering::Equal => self.num_moves.cmp(&other.num_moves),
      x => x,
    }
  }
}

/// * `graph` - interference graph
/// * `moves` - move graph
/// * `locals` - a slice of all local variables in this block
/// * `reg_colors` - a map of colors of all available registers
fn color_graph(
  graph: &LocationGraph,
  moves: &LocationGraph,
  locals: &[Var],
  reg_colors: &HashMap<Reg, i32>,
) -> IndexMap<Var, Color> {
  let mut queue = PriorityQueue::<NodeIndex, NodeState>::new();

  for node in graph.graph.node_indices() {
    let location = graph.graph[node];
    let move_node = moves.get_node(location);
    let num_moves =
      move_node.map_or(0, |node| moves.graph.neighbors(node).count());
    queue.push(node, NodeState::new(location, num_moves));
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
    for &ix in &state.saturation {
      if ix < 0 {
        continue;
      }
      if ix == last_color_ix + 1 {
        last_color_ix = ix;
      } else {
        break;
      }
    }
    let mut color = Color(last_color_ix + 1);

    // move biasing
    let color_is_reg = (color.0 as usize) < reg_colors.len();
    for neighbor in moves.graph.neighbors(node) {
      if let Some(neighbor) = queue.get(&neighbor) {
        if let Some(neighbor_color) = neighbor.1.color {
          let ok = if color_is_reg {
            (neighbor_color.0 as usize) < reg_colors.len()
          } else {
            (neighbor_color.0 as usize) >= reg_colors.len()
          };
          if ok && !state.saturation.contains(&neighbor_color.0) {
            color = neighbor_color;
            break;
          }
        }
      }
    }

    for neighbor in graph.graph.neighbors(node) {
      queue.change_priority_by(&neighbor, |v| {
        v.saturation.insert(color.0);
      })
    }
    let var = state.location.to_var().unwrap();
    var_colors.insert(var, color);
  }

  for &var in locals {
    var_colors.entry(var).or_insert(Color(0));
  }

  var_colors
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::location_set::LocationSet;
  use asm::{Arg, Block, Label};
  use ch2::pass::select_instruction::Info as OldOldInfo;
  use indexmap::indexset;
  use insta::assert_snapshot;
  use maplit::hashmap;

  fn var(name: &str) -> Arg<IdxVar> {
    Arg::Var(IdxVar::new(name))
  }

  #[test]
  fn example_in_book() {
    use asm::Reg::*;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Mov {
        src: Imm(1),
        dest: var("v"),
      },
      Mov {
        src: Imm(42),
        dest: var("w"),
      },
      Mov {
        src: var("v"),
        dest: var("x"),
      },
      Add {
        src: Imm(7),
        dest: var("x"),
      },
      Mov {
        src: var("x"),
        dest: var("y"),
      },
      Mov {
        src: var("x"),
        dest: var("z"),
      },
      Add {
        src: var("w"),
        dest: var("z"),
      },
      Mov {
        src: var("y"),
        dest: var("t"),
      },
      Neg(var("t")),
      Mov {
        src: var("z"),
        dest: Reg(Rax),
      },
      Add {
        src: var("t"),
        dest: Reg(Rax),
      },
      Jmp(Label::Conclusion),
    ];
    let label_live = hashmap! {
      Label::Conclusion => {
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
      blocks: vec![(
        Label::Start,
        Block {
          global: false,
          code,
        },
      )],
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
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
      Mov {
        src: Reg(Rbx),
        dest: var("w"),
      },
      Call("foo".to_owned(), 3),
      Add {
        src: Reg(Rax),
        dest: var("w"),
      },
      Jmp(Label::Conclusion)
    ];
    let label_live = hashmap! {
      Label::Conclusion => {
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
      blocks: vec![(
        Label::Start,
        Block {
          global: false,
          code,
        },
      )],
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
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
      Mov {
        src: Reg(Rbx),
        dest: var("w"),
      },
      Call("foo".to_owned(), 3),
      Add {
        src: Reg(Rax),
        dest: var("w"),
      },
      Jmp(Label::Conclusion)
    ];
    let label_live = hashmap! {
      Label::Conclusion => {
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
      blocks: vec![(
        Label::Start,
        Block {
          global: false,
          code,
        },
      )],
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
    let prog = super::super::interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn mov_same_variables() {
    use asm::Reg::*;
    use Instr::*;
    let code = vec![
      Mov {
        src: var("x"),
        dest: var("t"),
      },
      Add {
        src: var("y"),
        dest: var("t"),
      },
      Mov {
        src: var("t"),
        dest: var("z"),
      },
      Add {
        src: var("w"),
        dest: var("z"),
      },
      Neg(var("x")),
      Neg(var("y")),
      Neg(var("z")),
      Neg(var("w")),
    ];
    let label_live = hashmap! {};
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("t"),
          IdxVar::new("w"),
        },
      },
      blocks: vec![(
        Label::Start,
        Block {
          global: false,
          code,
        },
      )],
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
    let prog = super::super::interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn move_biasing_example_in_book() {
    use asm::Reg::*;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Mov {
        src: Imm(1),
        dest: var("v"),
      },
      Mov {
        src: Imm(42),
        dest: var("w"),
      },
      Mov {
        src: var("v"),
        dest: var("x"),
      },
      Add {
        src: Imm(7),
        dest: var("x"),
      },
      Mov {
        src: var("x"),
        dest: var("y"),
      },
      Mov {
        src: var("x"),
        dest: var("z"),
      },
      Add {
        src: var("w"),
        dest: var("z"),
      },
      Mov {
        src: var("y"),
        dest: var("t"),
      },
      Neg(var("t")),
      Mov {
        src: var("z"),
        dest: Reg(Rax),
      },
      Add {
        src: var("t"),
        dest: Reg(Rax),
      },
      Jmp(Label::Conclusion),
    ];
    let label_live = hashmap! {
      Label::Conclusion => {
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
      blocks: vec![(
        Label::Start,
        Block {
          global: false,
          code,
        },
      )],
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
    let prog = super::super::interference::build_interference(prog);
    let prog = super::super::move_biasing::build_move_graph(prog);
    let result = allocate_registers(prog, &[Rcx, Rdx, Rsi]);

    assert_snapshot!(result.to_string_pretty());
  }
}
