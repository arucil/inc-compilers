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
      Instr::Mov { src, dest } | Instr::Movzx { src, dest } => {
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
      Arg::ByteReg(reg) => {
        set.remove_reg((*reg).into());
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
      Arg::ByteReg(reg) => {
        set.add_reg((*reg).into());
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
  use asm::Label;
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
      blocks,
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
      blocks,
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
      blocks,
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
        locals: IndexSet::new(),
      },
      blocks,
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
      blocks,
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
      blocks,
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
      blocks,
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }
}
