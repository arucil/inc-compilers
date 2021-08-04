use super::liveness::Info as OldInfo;
use crate::location_set::{Location, LocationSet, VarStore};
use asm::{Arg, Block, Instr, Program, Reg};
use ast::IdxVar;
use bimap::BiMap;
use indexmap::{IndexMap, IndexSet};
use petgraph::graph::{Graph, NodeIndex, UnGraph};
use std::ops::{Deref, DerefMut};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub conflicts: IndexMap<String, InterferenceGraph>,
  pub var_store: VarStore,
}

pub struct InterferenceGraph {
  pub graph: UnGraph<Location, ()>,
  pub nodes: BiMap<Location, NodeIndex>,
}

pub fn build_interference(
  mut prog: Program<OldInfo, IdxVar>,
) -> Program<Info, IdxVar> {
  let var_store = &mut prog.info.var_store;
  let live = &prog.info.live;
  let conflicts = prog
    .blocks
    .iter()
    .map(|(name, block)| {
      (name.clone(), build_graph(block, &live[name], var_store))
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      conflicts,
      var_store: prog.info.var_store,
    },
    blocks: prog.blocks,
  }
}

fn build_graph(
  block: &Block<IdxVar>,
  live: &[LocationSet],
  var_store: &mut VarStore,
) -> InterferenceGraph {
  let mut graph = InterferenceGraph {
    graph: Graph::new_undirected(),
    nodes: BiMap::new(),
  };
  for (i, instr) in block.code.iter().enumerate() {
    add_instr_edges(instr, &live[i + 1], var_store, &mut graph);
    println!("{} {:?}\n{:?}\n------\n{:?}\n-----------------------------------", i, instr, graph.graph, var_store);
  }
  graph
}

fn add_instr_edges(
  instr: &Instr<IdxVar>,
  after: &LocationSet,
  var_store: &mut VarStore,
  graph: &mut InterferenceGraph,
) {
  let mut add = |write_loc: Location| {
    let write_loc_node = graph.get_node(write_loc);
    for after_loc in after {
      if after_loc != write_loc {
        let after_var_node = graph.get_node(after_loc);
        graph.add_edge(write_var_node, after_var_node, ());
      }
    }
  };

  match instr {
    Instr::Add(_, dest)
    | Instr::Sub(_, dest)
    | Instr::Neg(dest)
    | Instr::Pop(dest) => {
      if let Some(dest_var) = arg_to_var(dest, var_store) {
        add(dest_var);
      }
    }
    Instr::Call(_, _) => {
      add(Reg::Rax.into());
      add(Reg::Rcx.into());
      add(Reg::Rdx.into());
      add(Reg::Rsi.into());
      add(Reg::Rdi.into());
      add(Reg::R8.into());
      add(Reg::R9.into());
      add(Reg::R10.into());
      add(Reg::R11.into());
    }
    Instr::Mov(src, dest) => {
      if let Some(dest_var) = arg_to_var(dest, var_store) {
        let src_var = arg_to_var(src, var_store);
        let write_var_node = graph.get_node(dest_var);
        for after_var in after {
          if after_var != dest_var && Some(after_var) != src_var {
            let after_var_node = graph.get_node(after_var);
            graph.add_edge(write_var_node, after_var_node, ());
          }
        }
      }
    }
    Instr::Push(_) | Instr::Ret | Instr::Syscall | Instr::Jmp(_) => {}
    _ => {}
  }
}

fn arg_to_var(arg: &Arg<IdxVar>, var_store: &mut VarStore) -> Option<Var> {
  match arg {
    Arg::Deref(reg, _) | Arg::Reg(reg) => Some((*reg).into()),
    Arg::Var(var) => Some(var_store.get(var.clone())),
    Arg::Imm(_) => None,
  }
}

impl InterferenceGraph {
  fn get_node(&mut self, loc: Location) -> NodeIndex {
    if let Some(&node) = self.nodes.get_by_left(&var) {
      node
    } else {
      let node = self.graph.add_node(var);
      self.nodes.insert(var, node);
      node
    }
  }
}

impl Deref for InterferenceGraph {
  type Target = UnGraph<Var, ()>;

  fn deref(&self) -> &Self::Target {
    &self.graph
  }
}

impl DerefMut for InterferenceGraph {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.graph
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::Block;
  use ch2::pass::instruction::Info as OldOldInfo;
  use insta::assert_snapshot;
  use maplit::hashmap;
  use petgraph::dot::{Config, Dot};

  trait ShowConflicts {
    fn show(&self) -> String;
  }

  impl ShowConflicts for Program<Info, IdxVar> {
    fn show(&self) -> String {
      let graph = self.info.conflicts["start"]
        .graph
        .map(|_, var| var.to_arg(&self.info.var_store), |_, _| ());
      format!("{:?}", Dot::with_config(&graph, &[Config::EdgeNoLabel]))
    }
  }

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
    let result = build_interference(prog);

    assert_snapshot!(result.show());
  }
}
