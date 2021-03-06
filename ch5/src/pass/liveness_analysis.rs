use asm::{Block, Fun, Instr, Label, Program};
use ast::IdxVar;
use ch3::location_set::{LocationSet, VarStore};
use ch3::pass::liveness_analysis::AnalysisState;
use ch4::pass::instruction_selection::Info as OldInfo;
use ch4::pass::liveness_analysis::Info as NewInfo;
use indexmap::IndexMap;
use petgraph::graph::NodeIndex;
use petgraph::{Direction, Graph};
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

/// `label_live` is a map from labels to sets of live locations before the first
/// instruction of the blocks.
pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  label_live: HashMap<Label, LocationSet>,
) -> Program<NewInfo, IdxVar> {
  let funs = prog
    .funs
    .into_iter()
    .map(|fun| Fun {
      info: analyze_body(fun.info, &fun.blocks, Default::default()),
      ..fun
    })
    .collect();
  let info = analyze_body(prog.info, &prog.blocks, label_live);
  Program { info, funs, ..prog }
}

pub fn analyze_body(
  info: OldInfo,
  body: &[Block<IdxVar>],
  label_live: HashMap<Label, LocationSet>,
) -> NewInfo {
  let mut var_store = VarStore::new();
  for var in info.locals.keys() {
    var_store.insert(var.clone());
  }

  let blocks: HashMap<_, _> = body
    .iter()
    .map(|block| (block.label, block))
    .collect();
  let mut graph = Graph::<Label, ()>::new();
  let mut nodes = HashMap::<Label, NodeIndex>::new();
  for block in body {
    let ix = graph.add_node(block.label);
    nodes.insert(block.label, ix);
  }

  for node in nodes.values() {
    let last = blocks[&graph[*node]].code.last().unwrap().clone();
    if let Instr::LocalJmp(label) = last {
      if let Some(&node1) = nodes.get(&label) {
        graph.add_edge(node1, *node, ());
      }
      let code = &blocks[&graph[*node]].code;
      if code.len() > 1 {
        if let Instr::JumpIf { label, .. } = code[code.len() - 2].clone() {
          graph.add_edge(nodes[&label], *node, ());
        }
      }
    }
  }

  let state = AnalysisState::new(&var_store);
  let mut live = IndexMap::<Label, Vec<LocationSet>>::new();

  analyze_dataflow(
    label_live,
    graph,
    |mapping, label, _| {
      let l = state.block_liveness(blocks[&label], mapping);
      let output = l[0].clone();
      live.insert(label, l);
      output
    },
    LocationSet::new(),
    |mut set1, set2| {
      set1 |= set2;
      set1
    },
  );

  NewInfo {
    locals: info.locals,
    live,
    var_store,
  }
}

/// `mapping` - initial map from labels to live label sets
///
/// `g` - control flow graph
///
/// `transfer` - apply the analysis to one block
///
/// `bottom` - bottom for the lattice of abstract states (location sets)
///
/// `join` - join operator for the lattice of abstract states
pub fn analyze_dataflow<N, T, F, J>(
  mut mapping: HashMap<N, T>,
  g: Graph<N, ()>,
  mut transfer: F,
  bottom: T,
  join: J,
) -> HashMap<N, T>
where
  N: Clone + Hash + Eq,
  T: Clone + PartialEq,
  F: FnMut(&HashMap<N, T>, N, T) -> T,
  J: Fn(T, &T) -> T,
{
  for label in g.node_weights() {
    mapping.insert(label.clone(), bottom.clone());
  }

  let mut worklist: VecDeque<_> = g.node_indices().collect();
  while let Some(node_ix) = worklist.pop_front() {
    let label = g[node_ix].clone();
    let input = g
      .neighbors_directed(node_ix, Direction::Incoming)
      .map(|node_ix| &mapping[&g[node_ix]])
      .fold(bottom.clone(), &join);
    let output = transfer(&mapping, label.clone(), input);
    if output != mapping[&label] {
      mapping.insert(label, output);
      for node_ix in g.neighbors_directed(node_ix, Direction::Outgoing) {
        worklist.push_back(node_ix);
      }
    }
  }

  mapping
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::Label;
  use ast::Type;
  use indexmap::indexmap;
  use insta::assert_snapshot;
  use maplit::hashmap;
  use std::fmt::Write;

  trait ShowLiveness {
    fn show(&self) -> String;
  }

  impl ShowLiveness for Program<NewInfo, IdxVar> {
    fn show(&self) -> String {
      let mut buf = String::new();
      for block in &self.blocks {
        writeln!(&mut buf, "{}:", block.label).unwrap();
        let live = &self.info.live[&block.label];
        for (i, l) in live.iter().enumerate().take(block.code.len()) {
          buf += "                    ";
          l.write(&mut buf, &self.info.var_store).unwrap();
          buf += "\n";
          writeln!(&mut buf, "    {}", block.code[i]).unwrap();
        }
        buf += "                    ";
        live
          .last()
          .unwrap()
          .write(&mut buf, &self.info.var_store)
          .unwrap();
        buf += "\n\n";
      }
      buf
    }
  }

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
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("v") => Type::Int,
          IdxVar::new("w") => Type::Int,
          IdxVar::new("x") => Type::Int,
          IdxVar::new("y") => Type::Int,
          IdxVar::new("z") => Type::Int,
          IdxVar::new("t") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn push_pop() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
      start:
        push x
        mov w, rbx
        pop rbx
        add x, w
        jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Int,
          IdxVar::new("w") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
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
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Int,
          IdxVar::new("w") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn epilogue() {
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
      start:
        mov rsp, rbp
        pop rbp
        call print_int
        call print_newline
        mov rax, 60
        mov rdi, 0
        syscall
    "#,
    );
    let label_live = HashMap::new();
    let prog = Program {
      info: OldInfo {
        locals: indexmap! {},
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn nested_if() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
      start:
        call read_int
        mov x, rax
        call read_int
        mov y, rax
        cmp x, 1
        jl block2
        jmp block3
      block0:
        mov rax, y
        add rax, 2
        jmp conclusion
      block1:
        mov rax, z
        add rax, 10
        jmp conclusion
      block2:
        cmp x, 0
        je block0
        jmp block1
      block3:
        cmp x, 2
        je block0
        jmp block1
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Int,
          IdxVar::new("y") => Type::Int,
          IdxVar::new("z") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn if_in_init() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
start:
    call read_int
    mov tmp.0, rax
    cmp tmp.0, 3
    jge block3
    jmp block4
block0:
    mov rax, 2
    jmp conclusion
block1:
    mov rax, 41
    jmp conclusion
block2:
    cmp x.0, 10
    sete al
    movzx tmp.1, al
    cmp tmp.1, 0
    je block1
    jmp block0
block3:
    mov x.0, 10
    jmp block2
block4:
    mov x.0, 77
    jmp block2

    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("tmp.0") => Type::Int,
          IdxVar::new("x.0"  ) => Type::Int,
          IdxVar::new("tmp.1") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn complex_if() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
start:
    call read_int
    mov x.0, rax
    cmp x.0, 100
    jg block8
    jmp block9
block0:
    mov rax, -1
    jmp conclusion
block1:
    mov rax, y.1
    neg rax
    jmp conclusion
block2:
    mov tmp.1, y.1
    sub tmp.1, x.0
    cmp tmp.1, 10
    jl block0
    jmp block1
block3:
    cmp x.0, y.1
    jl block2
    jmp block1
block4:
    mov tmp.0, x.0
    sub tmp.0, y.1
    cmp tmp.0, 10
    jl block0
    jmp block3
block5:
    mov rax, 5000
    jmp conclusion
block6:
    mov rax, x.0
    jmp conclusion
block7:
    cmp x.0, 60
    jl block5
    jmp block6
block8:
    call read_int
    mov y.1, rax
    cmp x.0, y.1
    jge block4
    jmp block3
block9:
    cmp x.0, 40
    jg block7
    jmp block6
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("x.0"  ) => Type::Int,
          IdxVar::new("y.1"  ) => Type::Int,
          IdxVar::new("tmp.1") => Type::Int,
          IdxVar::new("tmp.0") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn ch5_example_in_book() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
start:
    mov sum.0, 0
    mov i.0, 5
    jmp block5
block5:
    mov tmp.3, i.0
    cmp tmp.3, 0
    jg block7
    jmp block8
block7:
    add sum.0, i.0
    mov tmp.4, 1
    neg tmp.4
    add tmp.4, i.0
    jmp block5
block8:
    mov rax, 27
    add rax, sum.0
    jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Epilogue => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexmap! {
          IdxVar::new("sum.0") => Type::Int,
          IdxVar::new("i.0"  ) => Type::Int,
          IdxVar::new("tmp.3") => Type::Int,
          IdxVar::new("tmp.4") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }
}
