use super::interference::{Info as OldInfo, Interference, Moves};
use crate::location_graph::{LocationGraph, NodeIndex};
use crate::location_set::{Location, Var, VarStore};
use asm::{Arg, Block, Instr, LabelOrArg, Program, Reg};
use ast::IdxVar;
use indexmap::{IndexMap, IndexSet};
use priority_queue::PriorityQueue;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
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
      .map(|block| alloc.allocate_block_registers(block))
      .collect();

    used_callee_saved_regs = alloc.used_callee_saved_regs;
  }

  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: num_locals * 8,
      used_callee_saved_regs,
    },
    blocks,
    funs: vec![],
    ..prog
  }
}

pub struct RegisterAlloc<'a, F> {
  pub num_locals: usize,
  pub conflicts: &'a LocationGraph<Interference>,
  pub moves: &'a LocationGraph<Moves>,
  pub reg_colors: &'a HashMap<Reg, u32>,
  pub available_regs: &'a [Reg],
  pub used_callee_saved_regs: IndexSet<Reg>,
  pub assign_instr_registers: F,
}

impl<'a, F> RegisterAlloc<'a, F>
where
  F: FnMut(Instr<IdxVar>, &IndexMap<Var, Color>) -> Instr,
{
  pub fn allocate_block_registers(&mut self, block: Block<IdxVar>) -> Block {
    let var_colors =
      color_graph(self.conflicts, self.moves, self.num_locals, self.reg_colors);

    for (_, c) in &var_colors {
      if let Some(&reg) = self.available_regs.get(c.0 as usize) {
        if reg.is_callee_saved() {
          self.used_callee_saved_regs.insert(reg);
        }
      }
    }

    Block {
      code: block
        .code
        .into_iter()
        .map(|instr| (self.assign_instr_registers)(instr, &var_colors))
        .collect(),
      ..block
    }
  }
}

pub fn gen_assign_instr_registers<'a>(
  var_store: &'a VarStore,
  available_regs: &'a [Reg],
  num_vars: &'a mut usize,
) -> impl (FnMut(Instr<IdxVar>, &IndexMap<Var, Color>) -> Instr) + 'a {
  let mut vars = HashSet::new();

  move |instr, var_colors| {
    let mut assign = |arg: Arg<IdxVar>| match arg {
      Arg::Deref(reg, i) => Arg::Deref(reg, i),
      Arg::Reg(reg) => Arg::Reg(reg),
      Arg::Imm(i) => Arg::Imm(i),
      Arg::Var(var) => {
        let i = var_colors[&var_store.get(var.clone())].index();
        if let Some(&reg) = available_regs.get(i) {
          Arg::Reg(reg)
        } else {
          if vars.insert(var) {
            *num_vars += 1;
          }
          let local = i - available_regs.len() + 1;
          Arg::Deref(Reg::Rbp, -8 * local as i32)
        }
      }
      Arg::ByteReg(reg) => Arg::ByteReg(reg),
    };

    match instr {
      Instr::Add { src, dest } => Instr::Add {
        src: assign(src),
        dest: assign(dest),
      },
      Instr::Call {
        label: LabelOrArg::Label(label),
        arity,
        gc,
      } => Instr::Call {
        label: LabelOrArg::Label(label),
        arity,
        gc,
      },
      Instr::Call {
        label: LabelOrArg::Arg(arg),
        arity,
        gc,
      } => Instr::Call {
        label: LabelOrArg::Arg(assign(arg)),
        arity,
        gc,
      },
      Instr::LocalJmp(label) => Instr::LocalJmp(label),
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
      Instr::Lea { label, dest } => Instr::Lea {
        label,
        dest: assign(dest),
      },
      _ => unreachable!("{}", instr),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Color(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct NodeState {
  location: Location,
  saturation: Saturation,
  num_moves: usize,
}

impl Color {
  pub fn index(&self) -> usize {
    self.0 as usize
  }
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
  num_locals: usize,
  reg_colors: &HashMap<Reg, u32>,
) -> IndexMap<Var, Color> {
  let mut queue = PriorityQueue::<NodeIndex<Interference>, NodeState>::new();
  let mut assigned_nodes = HashMap::<NodeIndex<Moves>, Color>::new();

  for node in graph.node_indices() {
    let location = graph.node_data(node).unwrap();
    if location.to_reg().is_none() {
      let move_node = moves.node_index(location);
      let num_moves = move_node.map_or(0, |node| moves.neighbors(node).count());
      queue.push(node, NodeState::new(location, num_locals, num_moves));
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
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
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
      blocks,
      ..Program::default()
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
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {IdxVar::new("x"),IdxVar::new("w")},
      },
      blocks,
      ..Program::default()
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
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {IdxVar::new("x"),IdxVar::new("w")},
      },
      blocks,
      ..Program::default()
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
      blocks,
      ..Program::default()
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
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
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
      blocks,
      ..Program::default()
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
    let result = color_graph(&interference, &moves, locals.len(), &hashmap! {});

    assert_debug_snapshot!(result);
  }
}
