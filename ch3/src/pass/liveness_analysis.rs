use crate::location_set::{LocationSet, VarStore};
use asm::{Arg, Instr, Program, Label};
use ast::IdxVar;
use ch2::pass::select_instruction::Info as OldInfo;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashMap;

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  pub live: IndexMap<String, Vec<LocationSet>>,
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
        if label != "start" {
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

  fn var(name: &str) -> Arg<IdxVar> {
    Arg::Var(IdxVar::new(name))
  }

  #[test]
  fn example_in_book() {
    use asm::Reg::*;
    use asm::Label;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Mov {
        src: Imm(1),
        dest: var("v"),
      },
      Mov {
        src: Imm(42),
        dest: var("w"),
      },
      Mov {
        src: var("v"),
        dest: var("x"),
      },
      Add {
        src: Imm(7),
        dest: var("x"),
      },
      Mov {
        src: var("x"),
        dest: var("y"),
      },
      Mov {
        src: var("x"),
        dest: var("z"),
      },
      Add {
        src: var("w"),
        dest: var("z"),
      },
      Mov {
        src: var("y"),
        dest: var("t"),
      },
      Neg(var("t")),
      Mov {
        src: var("z"),
        dest: Reg(Rax),
      },
      Add {
        src: var("t"),
        dest: Reg(Rax),
      },
      Jmp(Label::Conclusion)
    ];
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
        "start".to_owned(),
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
    use Arg::*;
    use Instr::*;
    let code = vec![
      Push(var("x")),
      Mov {
        src: Reg(Rbx),
        dest: var("w"),
      },
      Pop(Reg(Rbx)),
      Add {
        src: var("w"),
        dest: var("x"),
      },
      Jmp(Label::Conclusion)
    ];
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
        "start".to_owned(),
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
    use Arg::*;
    use Instr::*;
    let code = vec![
      Pop(Reg(Rdi)),
      Pop(Reg(Rsi)),
      Push(var("x")),
      Mov {
        src: Reg(Rbx),
        dest: var("w"),
      },
      Call("foo".to_owned(), 3),
      Add {
        src: Reg(Rax),
        dest: var("w"),
      },
      Jmp(Label::Conclusion)
    ];
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
        "start".to_owned(),
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
    use asm::Reg::*;
    use Arg::*;
    use Instr::*;
    let code = vec![
      Mov {
        src: Reg(Rbp),
        dest: Reg(Rsp),
      },
      Pop(Reg(Rbp)),
      Call("print_int".to_owned(), 0),
      Call("print_newline".to_owned(), 0),
      Mov {
        src: Imm(60),
        dest: Reg(Rax),
      },
      Mov {
        src: Imm(0),
        dest: Reg(Rdi),
      },
      Syscall,
    ];
    let label_live = HashMap::new();
    let prog = Program {
      info: OldInfo {
        locals: IndexSet::new(),
      },
      blocks: vec![(
        "start".to_owned(),
        Block {
          global: false,
          code,
        },
      )],
    };
    let result = analyze_liveness(prog, label_live);

    assert_snapshot!(result.show());
  }
}
