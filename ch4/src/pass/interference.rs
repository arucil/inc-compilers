#[cfg(test)]
mod tests {
  use super::super::*;
  use asm::{Label, Program};
  use ast::IdxVar;
  use ch2::pass::instruction_selection::Info as OldOldInfo;
  use ch3::location_set::LocationSet;
  use ch3::pass::interference;
  use indexmap::indexset;
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
        locals: indexset! {
          IdxVar::new("v"),
          IdxVar::new("i"),
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
        locals: indexset! {
          IdxVar::new("x"),
          IdxVar::new("w"),
        },
      },
      constants: Default::default(),
      blocks,
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
        locals: indexset! {
          IdxVar::new("t"),
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("w"),
        },
      },
      constants: Default::default(),
      blocks,
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
        locals: indexset! {
          IdxVar::new("tmp.0"),
          IdxVar::new("x.0"),
          IdxVar::new("tmp.1"),
        },
      },
      constants: Default::default(),
      blocks,
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
        locals: indexset! {
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
        },
      },
      constants: Default::default(),
      blocks,
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
        locals: indexset! {
          IdxVar::new("x.0"),
          IdxVar::new("y.1"),
          IdxVar::new("tmp.1"),
          IdxVar::new("tmp.0"),
        },
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
