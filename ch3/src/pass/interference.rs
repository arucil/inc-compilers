use super::liveness::Info as OldInfo;
use crate::location_set::{Location, LocationSet, VarStore};
use asm::{Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::{IndexMap, IndexSet};
use petgraph::dot::{Config, Dot};
use petgraph::graph::{Graph, NodeIndex, UnGraph};
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub conflicts: IndexMap<String, InterferenceGraph>,
  pub var_store: VarStore,
}

pub struct InterferenceGraph {
  pub graph: UnGraph<Location, ()>,
  pub nodes: IndexMap<Location, NodeIndex>,
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
    nodes: IndexMap::new(),
  };
  for (i, instr) in block.code.iter().enumerate() {
    add_instr_edges(instr, &live[i + 1], var_store, &mut graph);
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
        let after_loc_node = graph.get_node(after_loc);
        graph.add_edge(write_loc_node, after_loc_node);
      }
    }
  };

  match instr {
    Instr::Add(_, dest)
    | Instr::Sub(_, dest)
    | Instr::Neg(dest)
    | Instr::Pop(dest) => {
      if let Some(dest_loc) = Location::from_arg(dest.clone(), var_store) {
        add(dest_loc);
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
      if let Some(dest_loc) = Location::from_arg(dest.clone(), var_store) {
        let src_loc = Location::from_arg(src.clone(), var_store);
        let dest_loc_node = graph.get_node(dest_loc);
        for after_loc in after {
          if after_loc != dest_loc && Some(after_loc) != src_loc {
            let after_loc_node = graph.get_node(after_loc);
            graph.add_edge(dest_loc_node, after_loc_node);
          }
        }
      }
    }
    Instr::Push(_) | Instr::Ret | Instr::Syscall | Instr::Jmp(_) => {}
    _ => {}
  }
}

impl InterferenceGraph {
  fn get_node(&mut self, loc: Location) -> NodeIndex {
    if let Some(&node) = self.nodes.get(&loc) {
      node
    } else {
      let node = self.graph.add_node(loc);
      self.nodes.insert(loc, node);
      node
    }
  }

  fn add_edge(&mut self, n1: NodeIndex, n2: NodeIndex) {
    if !self.graph.contains_edge(n1, n2) {
      self.graph.add_edge(n1, n2, ());
    }
  }
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let graph = self.conflicts["start"]
      .graph
      .map(|_, var| var.to_arg(&self.var_store), |_, _| ());
    Dot::with_config(&graph, &[Config::EdgeNoLabel]).fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::{Arg, Block};
  use ch2::pass::instruction::Info as OldOldInfo;
  use insta::assert_snapshot;
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
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
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
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
