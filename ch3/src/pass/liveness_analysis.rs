use crate::location_set::{LocationSet, VarStore};
use asm::{Arg, Instr, Label, Program};
use ast::IdxVar;
use ch2::pass::select_instruction::Info as OldInfo;
use indexmap::{IndexMap, IndexSet};
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
    label_live: &label_live,
  };

  let live = prog
    .blocks
    .iter()
    .map(|(label, block)| {
      let live = state.block_liveness(block);
      (label.clone(), live)
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      live,
      var_store: state.var_store,
    },
    constants: prog.constants,
    blocks: prog.blocks,
  }
}

struct AnalysisState<'a> {
  var_store: VarStore,
  label_live: &'a HashMap<Label, LocationSet>,
}

impl<'a> AnalysisState<'a> {
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
      Instr::Add { src, dest } | Instr::Sub { src, dest } => {
        self.add_arg(before, src);
        self.add_arg(before, dest);
      }
      Instr::Mov { src, dest } => {
        self.remove_arg(before, dest);
        self.add_arg(before, src);
      }
      Instr::Neg(dest) => {
        self.add_arg(before, dest);
      }
      Instr::Jmp(label) => {
        *before = self.label_live[label].clone();
      }
      Instr::Call { arity, .. } => {
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
  use insta::assert_snapshot;
  use maplit::hashmap;

  trait ShowLiveness {
    fn show(&self) -> String;
  }

  impl ShowLiveness for Program<Info, IdxVar> {
    fn show(&self) -> String {
      let mut buf = String::new();
      for (label, block) in &self.blocks {
        if *label != Label::Start {
          continue;
        }
        let live = &self.info.live[label];
        for i in 0..block.code.len() {
          buf += "                ";
          live[i].write(&mut buf, &self.info.var_store).unwrap();
          buf += "\n";
          buf += &format!("{:?}", block.code[i]);
          buf += "\n";
        }
        buf += "                ";
        live
          .last()
          .unwrap()
          .write(&mut buf, &self.info.var_store)
          .unwrap();
        buf += "\n";
      }
      buf
    }
  }

  #[test]
  fn example_in_book() {
    use asm::Label;
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
      constants: Default::default(),
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
      constants: Default::default(),
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
      constants: Default::default(),
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
      constants: Default::default(),
      blocks,
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }
}
