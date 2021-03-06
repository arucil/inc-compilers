use super::instruction_selection::Info as OldInfo;
use asm::{Block, Instr, Label, Program};
use ast::{IdxVar, Type};
use ch3::location_set::{LocationSet, VarStore};
use ch3::pass::liveness_analysis::AnalysisState;
use indexmap::IndexMap;
use petgraph::algo::toposort;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use std::collections::HashMap;

pub struct Info {
  pub locals: IndexMap<IdxVar, Type>,
  pub live: IndexMap<Label, Vec<LocationSet>>,
  /// Includes all locals.
  pub var_store: VarStore,
}

/// `label_live` is a map from labels to sets of live locations before the first
/// instruction of the blocks.
pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  mut label_live: HashMap<Label, LocationSet>,
) -> Program<Info, IdxVar> {
  assert!(prog.funs.is_empty());

  let mut var_store = VarStore::new();
  for var in prog.info.locals.keys() {
    var_store.insert(var.clone());
  }

  let state = AnalysisState::new(&var_store);

  let live = sort_blocks(&prog.blocks)
    .map(|(label, block)| {
      let live = state.block_liveness(block, &label_live);
      label_live.insert(label, live[0].clone());
      (label, live)
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      live,
      var_store,
    },
    funs: vec![],
    ..prog
  }
}

fn sort_blocks(
  blocks: &[Block<IdxVar>],
) -> impl Iterator<Item = (Label, &Block<IdxVar>)> {
  let mut graph = Graph::new();
  let mut nodes = HashMap::<Label, NodeIndex>::new();
  for block in blocks {
    let ix = graph.add_node((block.label, block));
    nodes.insert(block.label, ix);
  }

  for node in nodes.values() {
    let last = graph[*node].1.code.last().unwrap().clone();
    if let Instr::LocalJmp(label) = last {
      if let Some(&node1) = nodes.get(&label) {
        graph.add_edge(*node, node1, ());
      }
      let code = &graph[*node].1.code;
      if code.len() > 1 {
        if let Instr::JumpIf { label, .. } = code[code.len() - 2].clone() {
          graph.add_edge(*node, nodes[&label], ());
        }
      }
    }
  }

  toposort(&graph, None)
    .unwrap()
    .into_iter()
    .rev()
    .map(move |node| graph[node])
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::Label;
  use indexmap::indexmap;
  use insta::assert_snapshot;
  use maplit::hashmap;
  use std::fmt::Write;

  trait ShowLiveness {
    fn show(&self) -> String;
  }

  impl ShowLiveness for Program<Info, IdxVar> {
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
          IdxVar::new("x.0") => Type::Int,
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
          IdxVar::new("x.0") => Type::Int,
          IdxVar::new("y.1") => Type::Int,
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
}
