use super::liveness_analysis::Info as OldInfo;
use asm::Program;
use ast::{IdxVar, Type};
use ch3::location_graph::LocationGraph;
use ch3::location_set::{Location, VarStore};
use ch3::pass::interference::{Interference, Moves, State};
use indexmap::IndexMap;
use petgraph::dot::{Config, Dot};
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  /// Includes all locals.
  pub locals: IndexMap<IdxVar, Type>,
  /// Nodes include all locals.
  pub conflicts: LocationGraph<Interference>,
  pub moves: LocationGraph<Moves>,
  pub var_store: VarStore,
}

pub fn build_interference(
  prog: Program<OldInfo, IdxVar>,
) -> Program<Info, IdxVar> {
  let locals = &prog.info.locals;
  let types = &prog.types;
  let var_store = &prog.info.var_store;
  let live = &prog.info.live;
  let mut conflicts = LocationGraph::new();
  for (_, var) in var_store.iter() {
    conflicts.insert_node(Location::from(*var));
  }
  let mut state = State {
    var_is_ref: |var: &IdxVar| locals[var].is_ref(types),
    conflicts,
    var_store,
  };
  for (label, block) in &prog.blocks {
    state.build_graph(block, &live[label]);
  }

  Program {
    info: Info {
      conflicts: state.conflicts,
      locals: prog.info.locals,
      moves: LocationGraph::new(),
      var_store: prog.info.var_store,
    },
    ..prog
  }
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let graph = self
      .conflicts
      .graph()
      .map(|_, var| var.to_arg(&self.var_store), |_, _| ());
    Dot::with_config(&graph, &[Config::EdgeNoLabel]).fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use super::super::instruction_selection::Info as OldOldInfo;
  use super::super::*;
  use super::*;
  use asm::{Label, Program};
  use ch3::location_set::LocationSet;
  use indexmap::indexmap;
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("v") => Type::Int,
          IdxVar::new("i") => Type::Int,
          IdxVar::new("w") => Type::Int,
          IdxVar::new("x") => Type::Int,
          IdxVar::new("y") => Type::Int,
          IdxVar::new("z") => Type::Int,
          IdxVar::new("t") => Type::Int,
        },
      },
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Int,
          IdxVar::new("w") => Type::Int,
        },
      },
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

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
        locals: indexmap! {
          IdxVar::new("t") => Type::Int,
          IdxVar::new("x") => Type::Int,
          IdxVar::new("y") => Type::Int,
          IdxVar::new("z") => Type::Int,
          IdxVar::new("w") => Type::Int,
        },
      },
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("tmp.0") => Type::Int,
          IdxVar::new("x.0"  ) => Type::Int,
          IdxVar::new("tmp.1") => Type::Int,
        },
      },
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Int,
          IdxVar::new("y") => Type::Int,
          IdxVar::new("z") => Type::Int,
        },
      },
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("x.0"  ) => Type::Int,
          IdxVar::new("y.1"  ) => Type::Int,
          IdxVar::new("tmp.1") => Type::Int,
          IdxVar::new("tmp.0") => Type::Int,
        },
      },
      constants: Default::default(),
      externs: Default::default(),
      blocks,
      types: Default::default(),
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
