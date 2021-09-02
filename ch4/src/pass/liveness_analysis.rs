use asm::{Arg, Block, Instr, Label, Program};
use ast::IdxVar;
use ch2::pass::select_instruction::Info as OldInfo;
use ch3::location_set::{LocationSet, VarStore};
use indexmap::{IndexMap, IndexSet};
use petgraph::algo::toposort;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use std::collections::HashMap;

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub live: IndexMap<Label, Vec<LocationSet>>,
  pub var_store: VarStore,
}

pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  label_live: HashMap<Label, LocationSet>,
) -> Program<Info, IdxVar> {
  let mut state = AnalysisState {
    var_store: VarStore::new(),
    label_live,
  };

  let live = sort_blocks(&prog.blocks)
    .iter()
    .map(|(label, block)| {
      let live = state.block_liveness(block);
      state.label_live.insert(*label, live[0].clone());
      (label.clone(), live)
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      live,
      var_store: state.var_store,
    },
    blocks: prog.blocks,
  }
}

fn sort_blocks(
  blocks: &[(Label, Block<IdxVar>)],
) -> Vec<(Label, &Block<IdxVar>)> {
  let mut graph = Graph::new();
  let mut nodes = HashMap::<Label, NodeIndex>::new();
  for (label, block) in blocks {
    let ix = graph.add_node((*label, block));
    nodes.insert(*label, ix);
  }

  for (_, node) in &nodes {
    let last = graph[*node].1.code.last().unwrap().clone();
    if let Instr::Jmp(label) = last {
      if let Some(&node1) = nodes.get(&label) {
        graph.add_edge(*node, node1, ());
      }
      let code = &graph[*node].1.code;
      if code.len() > 1 {
        if let Instr::JumpIf(_, label) = code[code.len() - 2].clone() {
          graph.add_edge(*node, nodes[&label], ());
        }
      }
    }
  }

  toposort(&graph, None)
    .unwrap()
    .into_iter()
    .rev()
    .map(|node| graph[node])
    .collect()
}

struct AnalysisState {
  var_store: VarStore,
  label_live: HashMap<Label, LocationSet>,
}

impl AnalysisState {
  fn block_liveness(&mut self, block: &asm::Block<IdxVar>) -> Vec<LocationSet> {
    let mut set = vec![LocationSet::new(); block.code.len() + 2];
    for (i, ins) in block.code.iter().enumerate().rev() {
      set[i] = set[i + 1].clone();
      self.instr_liveness(ins, &mut set[i]);
    }
    set.pop();
    set
  }

  fn instr_liveness(&mut self, ins: &Instr<IdxVar>, before: &mut LocationSet) {
    match ins {
      Instr::Ret => {}
      Instr::Syscall => {}
      Instr::Pop(arg) => {
        self.remove_arg(before, arg);
      }
      Instr::Push(arg) => {
        self.add_arg(before, arg);
      }
      Instr::Add { src, dest }
      | Instr::Sub { src, dest }
      | Instr::Xor { src, dest } => {
        self.add_arg(before, src);
        self.add_arg(before, dest);
      }
      Instr::Mov { src, dest } | Instr::Movzb { src, dest } => {
        self.remove_arg(before, dest);
        self.add_arg(before, src);
      }
      Instr::Neg(dest) => {
        self.add_arg(before, dest);
      }
      Instr::Cmp { src, dest } => {
        self.add_arg(before, src);
        self.add_arg(before, dest);
      }
      Instr::Jmp(label) => {
        *before = self.label_live[label].clone();
      }
      Instr::SetIf(_, dest) => {
        self.remove_arg(before, dest);
      }
      Instr::JumpIf(_, label) => {
        *before |= &self.label_live[label];
      }
      Instr::Call(_, arity) => {
        before.remove_caller_saved_regs();
        before.add_argument_regs(*arity);
      }
      _ => unimplemented!("{:?}", ins),
    }
  }

  fn remove_arg(&mut self, set: &mut LocationSet, arg: &Arg<IdxVar>) {
    match arg {
      Arg::Deref(reg, _) | Arg::Reg(reg) => {
        set.remove_reg(*reg);
      }
      Arg::Var(var) => {
        let var = self.var_store.get(var.clone());
        set.remove_var(var);
      }
      _ => {}
    }
  }

  fn add_arg(&mut self, set: &mut LocationSet, arg: &Arg<IdxVar>) {
    match arg {
      Arg::Deref(reg, _) | Arg::Reg(reg) => {
        set.add_reg(*reg);
      }
      Arg::Var(var) => {
        let var = self.var_store.get(var.clone());
        set.add_var(var);
      }
      _ => {}
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use asm::Block;
  use insta::assert_snapshot;
  use maplit::hashmap;

  trait ShowLiveness {
    fn show(&self) -> String;
  }

  impl ShowLiveness for Program<Info, IdxVar> {
    fn show(&self) -> String {
      let mut buf = String::new();
      for (label, block) in &self.blocks {
        buf += &format!("{:?}:\n", label);
        let live = &self.info.live[label];
        for i in 0..block.code.len() {
          buf += "                    ";
          live[i].write(&mut buf, &self.info.var_store).unwrap();
          buf += "\n";
          buf += &format!("    {:?}", block.code[i]);
          buf += "\n";
        }
        buf += "                ";
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
    use asm::Label;
    use asm::Reg::*;
    let code = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
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
      Label::Conclusion => {
        let mut set = LocationSet::new();
        set.add_reg(Rax);
        set.add_reg(Rsp);
        set
      }
    };
    let prog = Program {
      info: OldInfo {
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
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn push_pop() {
    use asm::Reg::*;
    let code = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
      push x
      mov w, rbx
      pop rbx
      add x, w
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
      info: OldInfo {
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
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn call() {
    use asm::Reg::*;
    let code = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
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
      info: OldInfo {
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
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn epilogue() {
    let code = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
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
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }

  #[test]
  fn nested_if() {
    use asm::Reg::*;
    let start = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
      call read_int
      mov x, rax
      call read_int
      mov y, rax
      cmp x, 1
      jl block2
      jmp block3
    "#,
    );
    let block0 = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
      mov rax, y
      add rax, 2
      jmp conclusion
    "#,
    );
    let block1 = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
      mov rax, y
      add rax, 10
      jmp conclusion
    "#,
    );
    let block2 = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
      cmp x, 0
      je block0
      jmp block1
    "#,
    );
    let block3 = asm::parse_code(
      |s| IdxVar::new(s),
      r#"
      cmp x, 2
      je block0
      jmp block1
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
      info: OldInfo {
        locals: IndexSet::new(),
      },
      blocks: vec![
        (
          Label::Start,
          Block {
            global: false,
            code: start,
          },
        ),
        (
          Label::Tmp(0),
          Block {
            global: false,
            code: block0,
          },
        ),
        (
          Label::Tmp(1),
          Block {
            global: false,
            code: block1,
          },
        ),
        (
          Label::Tmp(2),
          Block {
            global: false,
            code: block2,
          },
        ),
        (
          Label::Tmp(3),
          Block {
            global: false,
            code: block3,
          },
        ),
      ],
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }
}
