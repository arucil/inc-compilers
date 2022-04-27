use super::interference::{Info as OldInfo, Interference, Moves};
use crate::location_graph::{LocationGraph, NodeIndex};
use crate::location_set::{Location, Var, VarStore};
use asm::{Arg, Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::{IndexMap, IndexSet};
use priority_queue::PriorityQueue;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
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
    .map(|(i, &reg)| (reg, i as _))
    .collect();
  let var_store = prog.info.var_store;
  let conflicts = prog.info.conflicts;
  let moves = prog.info.moves;
  let locals: Vec<_> = prog
    .info
    .locals
    .iter()
    .map(|var| var_store.get(var.clone()))
    .collect();
  let mut max_locals = 0;

  let mut alloc = RegisterAlloc {
    locals: &locals,
    var_store,
    conflicts: &conflicts,
    moves: &moves,
    reg_colors: &reg_colors,
    available_regs,
    used_callee_saved_regs: IndexSet::new(),
  };

  let blocks = prog
    .blocks
    .into_iter()
    .map(|(label, block)| {
      let (block, locals) = alloc.allocate_registers_block(block);
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
      used_callee_saved_regs: alloc.used_callee_saved_regs,
    },
    constants: prog.constants,
    blocks,
  }
}

struct RegisterAlloc<'a> {
  locals: &'a [Var],
  var_store: VarStore,
  conflicts: &'a LocationGraph<Interference>,
  moves: &'a LocationGraph<Moves>,
  reg_colors: &'a HashMap<Reg, u32>,
  available_regs: &'a [Reg],
  used_callee_saved_regs: IndexSet<Reg>,
}

impl<'a> RegisterAlloc<'a> {
  fn allocate_registers_block(
    &mut self,
    block: Block<IdxVar>,
  ) -> (Block, usize) {
    let var_colors =
      color_graph(self.conflicts, self.moves, self.locals, self.reg_colors);

    for (_, c) in &var_colors {
      if let Some(&reg) = self.available_regs.get(c.0 as usize) {
        if reg.is_callee_saved() {
          self.used_callee_saved_regs.insert(reg);
        }
      }
    }

    let block = Block {
      global: block.global,
      code: block
        .code
        .into_iter()
        .map(|instr| self.assign_instr_var(instr, &var_colors))
        .collect(),
    };

    let num_locals = var_colors
      .values()
      .filter(|c| c.0 as usize >= self.reg_colors.len())
      .collect::<HashSet<_>>()
      .len();

    (block, num_locals)
  }

  fn assign_instr_var(
    &mut self,
    instr: Instr<IdxVar>,
    var_colors: &IndexMap<Var, Color>,
  ) -> Instr {
    let assign = |arg: Arg<IdxVar>| match arg {
      Arg::Deref(reg, i) => Arg::Deref(reg, i),
      Arg::Reg(reg) => Arg::Reg(reg),
      Arg::Imm(i) => Arg::Imm(i),
      Arg::Var(var) => {
        let i = var_colors[&self.var_store.get(var)].0 as usize;
        if let Some(&reg) = self.available_regs.get(i) {
          Arg::Reg(reg)
        } else {
          let local = i - self.available_regs.len() + 1;
          Arg::Deref(Reg::Rbp, -8 * local as i32)
        }
      }
      Arg::ByteReg(reg) => Arg::ByteReg(reg),
      Arg::Label(l) => Arg::Label(l),
    };

    let instr = match instr {
      Instr::Add { src, dest } => Instr::Add {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Call { label, arity } => Instr::Call { label, arity },
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
      Instr::Cmp { src, dest } => Instr::Cmp {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::JumpIf { cmp, label } => Instr::JumpIf { cmp, label },
      Instr::Movzx { src, dest } => Instr::Movzx {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::SetIf { cmp, dest } => Instr::SetIf {
        cmp,
        dest: assign(dest),
      },
      Instr::Xor { src, dest } => Instr::Xor {
        src: assign(src),
        dest: assign(dest),
      },
      _ => unreachable!("{:?}", instr),
    };
    instr
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Color(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct NodeState {
  location: Location,
  saturation: Saturation,
  num_moves: usize,
}

impl NodeState {
  fn new(location: Location, locals: usize, num_moves: usize) -> Self {
    Self {
      location,
      saturation: Saturation::new(locals),
      num_moves,
    }
  }
}

impl PartialOrd for NodeState {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self
      .saturation
      .count()
      .partial_cmp(&other.saturation.count())
    {
      Some(Ordering::Equal) => self.num_moves.partial_cmp(&other.num_moves),
      x => x,
    }
  }
}

impl Ord for NodeState {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.saturation.count().cmp(&other.saturation.count()) {
      Ordering::Equal => self.num_moves.cmp(&other.num_moves),
      x => x,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Saturation {
  colors: Vec<u32>,
  negative_colors: u32,
}

impl Saturation {
  fn new(size: usize) -> Self {
    Self {
      colors: vec![0; (size + 31) >> 5],
      negative_colors: 0,
    }
  }

  fn count(&self) -> u32 {
    let mut count = 0;
    for &x in &self.colors {
      count += x.count_ones();
    }
    count + self.negative_colors
  }

  fn contains(&self, color: Color) -> bool {
    self.colors[(color.0 >> 5) as usize] & (1 << (color.0 & 31)) != 0
  }

  fn insert(&mut self, color: Color) {
    self.colors[(color.0 >> 5) as usize] |= 1 << (color.0 & 31);
  }

  fn first_free_color(&self) -> Color {
    for i in 0..self.colors.len() {
      let x = self.colors[i].trailing_ones();
      if x < 32 {
        return Color((i << 5) as u32 + x);
      }
    }
    unreachable!("no free color")
  }
}

/// * `graph` - interference graph
/// * `moves` - move graph
/// * `locals` - a slice of all local variables in this block
/// * `reg_colors` - a map of colors of all available registers
fn color_graph(
  graph: &LocationGraph<Interference>,
  moves: &LocationGraph<Moves>,
  locals: &[Var],
  reg_colors: &HashMap<Reg, u32>,
) -> IndexMap<Var, Color> {
  let mut queue = PriorityQueue::<NodeIndex<Interference>, NodeState>::new();
  let mut assigned_nodes = HashMap::<NodeIndex<Moves>, Color>::new();

  for node in graph.node_indices() {
    let location = graph.node_data(node).unwrap();
    if location.to_reg().is_none() {
      let move_node = moves.node_index(location);
      let num_moves = move_node.map_or(0, |node| moves.neighbors(node).count());
      queue.push(node, NodeState::new(location, locals.len(), num_moves));
    }
  }

  for node in graph.node_indices() {
    if graph.node_data(node).unwrap().to_reg().is_some() {
      for neighbor in graph.neighbors(node) {
        queue.change_priority_by(&neighbor, |v| {
          v.saturation.negative_colors += 1;
        })
      }
    }
    if let Some(reg) = graph.node_data(node).unwrap().to_reg() {
      if let Some(&color) = reg_colors.get(&reg) {
        for neighbor in graph.neighbors(node) {
          queue.change_priority_by(&neighbor, |v| {
            v.saturation.insert(Color(color));
          })
        }
      } else {
        for neighbor in graph.neighbors(node) {
          queue.change_priority_by(&neighbor, |v| {
            v.saturation.negative_colors += 1;
          })
        }
      }
    }
  }

  let mut var_colors = IndexMap::new();

  while let Some((node, state)) = queue.pop() {
    let mut color = state.saturation.first_free_color();

    // move biasing: prefer color that is move-related, but should not override
    // the preference for registers over stack locations.
    let color_is_reg = (color.0 as usize) < reg_colors.len();
    if let Some(node) = moves.node_index(state.location) {
      for neighbor in moves.neighbors(node) {
        if let Some(&neighbor_color) = assigned_nodes.get(&neighbor) {
          let neighbor_color_is_reg =
            (neighbor_color.0 as usize) < reg_colors.len();
          if color_is_reg == neighbor_color_is_reg
            && !state.saturation.contains(neighbor_color)
          {
            color = neighbor_color;
            break;
          }
        }
      }
      assigned_nodes.insert(node, color);
    }

    for neighbor in graph.neighbors(node) {
      queue.change_priority_by(&neighbor, |v| {
        v.saturation.insert(color);
      })
    }
    let var = state.location.to_var().unwrap();
    var_colors.insert(var, color);
  }

  // TODO remove this
  for &var in locals {
    var_colors.entry(var).or_insert(Color(0));
  }

  var_colors
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use crate::location_set::LocationSet;
  use asm::Label;
  use ch2::pass::instruction_selection::Info as OldOldInfo;
  use indexmap::indexset;
  use insta::{assert_debug_snapshot, assert_snapshot};
  use maplit::hashmap;

  #[test]
  fn example_in_book() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      mov v, 1
      mov w, 42
      mov x, v
      add x, 7
      mov y, x
      mov z, x
      add z, w
      mov t, y
      neg t
      mov rax, z
      add rax, t
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {
          IdxVar::new("v"),
          IdxVar::new("w"),
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("t")
        },
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rcx]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn call() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      pop rdi
      pop rsi
      push x
      mov w, rbx
      call foo, 3
      add w, rax
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {IdxVar::new("x"),IdxVar::new("w")},
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8, R12]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn call_no_enough_registers() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      pop rdi
      pop rsi
      push x
      mov w, rbx
      call foo, 3
      add w, rax
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {IdxVar::new("x"),IdxVar::new("w")},
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn mov_same_variables() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      mov t, x
      add t, y
      mov z, t
      add z, w
      neg x
      neg y
      neg z
      neg w
    "#,
    );
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
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let result = allocate_registers(prog, &[Rdx, Rdi, Rsi, R8]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn move_biasing_example_in_book() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      mov v, 1
      mov w, 42
      mov x, v
      add x, 7
      mov y, x
      mov z, x
      add z, w
      mov t, y
      neg t
      mov rax, z
      add rax, t
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {
          IdxVar::new("v"),
          IdxVar::new("w"),
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("t")
        },
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let prog = interference::build_interference(prog);
    let prog = move_biasing::build_move_graph(prog);
    let result = allocate_registers(prog, &[Rcx, Rdx, Rsi]);

    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn move_biasing_prefer_move_related_color() {
    let mut var_store = VarStore::new();
    let x = var_store.insert(IdxVar::new("x"));
    let y = var_store.insert(IdxVar::new("y"));
    let z = var_store.insert(IdxVar::new("z"));
    let w = var_store.insert(IdxVar::new("w"));
    let v = var_store.insert(IdxVar::new("v"));
    let interference = {
      let mut g = LocationGraph::new();
      let x_node = g.insert_node(Location::from(x));
      let y_node = g.insert_node(Location::from(y));
      let z_node = g.insert_node(Location::from(z));
      let w_node = g.insert_node(Location::from(w));
      let _v_node = g.insert_node(Location::from(v));
      let rax_node = g.insert_node(Location::from(Reg::Rax));
      let rsp_node = g.insert_node(Location::from(Reg::Rsp));
      g.add_edge(x_node, z_node);
      g.add_edge(y_node, w_node);
      g.add_edge(z_node, rax_node);
      g.add_edge(z_node, rsp_node);
      g.add_edge(x_node, rsp_node);
      g
    };
    let moves = {
      let mut g = LocationGraph::new();
      let x_node = g.insert_node(Location::from(x));
      let y_node = g.insert_node(Location::from(y));
      let v_node = g.insert_node(Location::from(v));
      g.add_edge(x_node, y_node);
      g.add_edge(x_node, v_node);
      g
    };
    let locals = vec![x, y, z, w, v];
    let result = color_graph(&interference, &moves, &locals, &hashmap! {});

    assert_debug_snapshot!(result);
  }
}
