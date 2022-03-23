use super::liveness_analysis::Info as OldInfo;
use crate::location_graph::LocationGraph;
use crate::location_set::{Location, LocationSet, VarStore};
use asm::{Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::IndexSet;
use petgraph::dot::{Config, Dot};
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub conflicts: LocationGraph,
  pub moves: LocationGraph,
  pub var_store: VarStore,
}

pub fn build_interference(
  mut prog: Program<OldInfo, IdxVar>,
) -> Program<Info, IdxVar> {
  let var_store = &mut prog.info.var_store;
  let live = &prog.info.live;
  let mut conflicts = LocationGraph::new();
  for (label, block) in &prog.blocks {
    build_graph(block, &live[label], var_store, &mut conflicts);
  }

  Program {
    info: Info {
      locals: prog.info.locals,
      conflicts,
      moves: LocationGraph::new(),
      var_store: prog.info.var_store,
    },
    constants: prog.constants,
    blocks: prog.blocks,
  }
}

fn build_graph(
  block: &Block<IdxVar>,
  live: &[LocationSet],
  var_store: &mut VarStore,
  graph: &mut LocationGraph,
) {
  for (i, instr) in block.code.iter().enumerate() {
    add_instr_edges(instr, &live[i + 1], var_store, graph);
  }
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
    Instr::Call { .. } => {
      for reg in Reg::caller_saved_regs() {
        add(reg.into());
      }
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
    let graph = self
      .conflicts
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
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
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
    "#,
    );
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
      constants: Default::default(),
      blocks,
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
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
      constants: Default::default(),
      blocks,
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }

  #[test]
  fn mov_same_variables() {
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      mov t, x
      add t, y
      mov z, t
      add z, w
    "#,
    );
    let label_live = hashmap! {};
    let prog = Program {
      info: OldOldInfo {
        locals: IndexSet::new(),
      },
      constants: Default::default(),
      blocks,
    };
    let prog =
      super::super::liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
