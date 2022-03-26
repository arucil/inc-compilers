use super::liveness_analysis::Info as OldInfo;
use asm::{Block, Instr, Program, Reg};
use ast::IdxVar;
use ch3::location_graph::LocationGraph;
use ch3::location_set::{Location, LocationSet, VarStore};
use ch3::pass::interference::{Info, Interference};

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
  graph: &mut LocationGraph<Interference>,
) {
  for (i, instr) in block.code.iter().enumerate() {
    add_instr_edges(instr, &live[i + 1], var_store, graph);
  }
}

fn add_instr_edges(
  instr: &Instr<IdxVar>,
  after: &LocationSet,
  var_store: &mut VarStore,
  graph: &mut LocationGraph<Interference>,
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
    | Instr::Xor { dest, .. }
    | Instr::SetIf { dest, .. }
    | Instr::Neg(dest)
    | Instr::Pop(dest) => {
      if let Some(dest_loc) = Location::from_arg(dest.clone(), var_store) {
        add(dest_loc);
      }
    }
    Instr::Call { .. } => {
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
    Instr::Mov { src, dest } | Instr::Movzx { src, dest } => {
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
    Instr::Push(_)
    | Instr::Ret
    | Instr::Syscall
    | Instr::Jmp(_)
    | Instr::JumpIf { .. }
    | Instr::Cmp { .. } => {}
    _ => {}
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use asm::Label;
  use ch2::pass::select_instruction::Info as OldOldInfo;
  use indexmap::IndexSet;
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
        locals: IndexSet::new(),
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
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
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: IndexSet::new(),
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
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
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

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
        locals: IndexSet::new(),
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

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
        locals: IndexSet::new(),
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

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
        locals: IndexSet::new(),
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
