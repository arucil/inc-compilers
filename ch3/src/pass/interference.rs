use super::liveness_analysis::Info as OldInfo;
use crate::location_graph::LocationGraph;
use crate::location_set::{Location, LocationSet, VarStore};
use asm::{Block, Instr, Program, Reg, Label};
use ast::IdxVar;
use indexmap::{IndexMap, IndexSet};
use petgraph::dot::{Config, Dot};
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub conflicts: IndexMap<Label, LocationGraph>,
  pub moves: IndexMap<Label, LocationGraph>,
  pub var_store: VarStore,
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
      moves: IndexMap::new(),
      var_store: prog.info.var_store,
    },
    blocks: prog.blocks,
  }
}

fn build_graph(
  block: &Block<IdxVar>,
  live: &[LocationSet],
  var_store: &mut VarStore,
) -> LocationGraph {
  let mut graph = LocationGraph::new();
  for (i, instr) in block.code.iter().enumerate() {
    add_instr_edges(instr, &live[i + 1], var_store, &mut graph);
  }
  graph
}

fn add_instr_edges(
  instr: &Instr<IdxVar>,
  after: &LocationSet,
  var_store: &mut VarStore,
  graph: &mut LocationGraph,
) {
  let mut add = |write_loc: Location| {
    let write_loc_node = graph.insert_node(write_loc);
    for after_loc in after {
      if after_loc != write_loc {
        let after_loc_node = graph.insert_node(after_loc);
        graph.add_edge(write_loc_node, after_loc_node);
      }
    }
  };

  match instr {
    Instr::Add { dest, .. }
    | Instr::Sub { dest, .. }
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
    Instr::Mov { src, dest } => {
      if let Some(dest_loc) = Location::from_arg(dest.clone(), var_store) {
        let src_loc = Location::from_arg(src.clone(), var_store);
        let dest_loc_node = graph.insert_node(dest_loc);
        for after_loc in after {
          if after_loc != dest_loc && Some(after_loc) != src_loc {
            let after_loc_node = graph.insert_node(after_loc);
            graph.add_edge(dest_loc_node, after_loc_node);
          }
        }
      }
    }
    Instr::Push(_) | Instr::Ret | Instr::Syscall | Instr::Jmp(_) => {}
    _ => {}
  }
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let graph = self.conflicts[&Label::Start]
      .graph
      .map(|_, var| var.to_arg(&self.var_store), |_, _| ());
    Dot::with_config(&graph, &[Config::EdgeNoLabel]).fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::Label;
  use ch2::pass::select_instruction::Info as OldOldInfo;
  use insta::assert_snapshot;
  use maplit::hashmap;

  #[test]
  fn example_in_book() {
    use asm::Reg::*;
    let code = asm::parse_code(|s| IdxVar::new(s), r#"
      mov v, i
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
    "#);
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
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }

  #[test]
  fn call() {
    use asm::Reg::*;
    let code = asm::parse_code(|s| IdxVar::new(s), r#"
      pop rdi
      pop rsi
      push x
      mov w, rbx
      call foo, 3
      add w, rax
      jmp conclusion
    "#);
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
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }

  #[test]
  fn mov_same_variables() {
    let code = asm::parse_code(|s| IdxVar::new(s), r#"
      mov t, x
      add t, y
      mov z, t
      add z, w
    "#);
    let label_live = hashmap! {};
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
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
