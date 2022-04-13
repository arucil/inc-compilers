use crate::location_set::{LocationSet, VarStore};
use asm::{Arg, Instr, Label, Program, Reg};
use ast::IdxVar;
use ch2::pass::select_instruction::Info as OldInfo;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub live: IndexMap<Label, Vec<LocationSet>>,
  /// Includes all locals.
  pub var_store: VarStore,
}

/// `label_live` is a map from labels to sets of live locations before the first
/// instruction of the blocks.
pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  label_live: HashMap<Label, LocationSet>,
) -> Program<Info, IdxVar> {
  let mut var_store = VarStore::new();
  for var in &prog.info.locals {
    var_store.insert(var.clone());
  }

  let state = AnalysisState {
    var_store: &var_store,
  };

  let live = prog
    .blocks
    .iter()
    .map(|(label, block)| {
      let live = state.block_liveness(block, &label_live);
      (label.clone(), live)
    })
    .collect();

  Program {
    info: Info {
      locals: prog.info.locals,
      live,
      var_store,
    },
    constants: prog.constants,
    blocks: prog.blocks,
  }
}

pub struct AnalysisState<'a> {
  pub var_store: &'a VarStore,
}

impl<'a> AnalysisState<'a> {
  pub fn block_liveness(
    &self,
    block: &asm::Block<IdxVar>,
    label_live: &HashMap<Label, LocationSet>,
  ) -> Vec<LocationSet> {
    let mut set = vec![LocationSet::new(); block.code.len() + 1];
    for (i, ins) in block.code.iter().enumerate().rev() {
      set[i] = set[i + 1].clone();
      self.instr_liveness(ins, &mut set[i], label_live);
    }
    set
  }

  fn instr_liveness(
    &self,
    ins: &Instr<IdxVar>,
    before: &mut LocationSet,
    label_live: &HashMap<Label, LocationSet>,
  ) {
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
      Instr::Mov { src, dest } => {
        if src != dest {
          self.remove_arg(before, dest);
          self.add_arg(before, src);
        }
      }
      Instr::Movzx { src, dest } => {
        if src.bytereg_to_reg() != dest.bytereg_to_reg() {
          self.remove_arg(before, dest);
          self.add_arg(before, src);
        }
      }
      Instr::Neg(dest) => {
        self.add_arg(before, dest);
      }
      Instr::Cmp { src, dest } => {
        self.add_arg(before, src);
        self.add_arg(before, dest);
      }
      Instr::SetIf { dest, .. } => {
        self.remove_arg(before, dest);
      }
      Instr::JumpIf { label, .. } => {
        *before |= &label_live[label];
      }
      Instr::Jmp(label) => {
        *before = label_live[label].clone();
      }
      Instr::Call { arity, .. } => {
        assert!(*arity <= 6);
        for reg in Reg::caller_saved_regs() {
          before.remove_reg(reg);
        }
        for reg in Reg::argument_regs().into_iter().take(*arity) {
          before.add_reg(reg);
        }
      }
      _ => unimplemented!("{:?}", ins),
    }
  }

  fn remove_arg(&self, set: &mut LocationSet, arg: &Arg<IdxVar>) {
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

  fn add_arg(&self, set: &mut LocationSet, arg: &Arg<IdxVar>) {
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
  use indexmap::indexset;
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexset! {
          IdxVar::new("v"),
          IdxVar::new("w"),
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("t"),
        },
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexset! {
          IdxVar::new("x"),
          IdxVar::new("w"),
        },
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldInfo {
        locals: indexset! {
          IdxVar::new("x"),
          IdxVar::new("w"),
        },
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
