#[cfg(test)]
mod tests {
  use asm::{Label, Program};
  use ast::{IdxVar, Type};
  use ch3::location_set::LocationSet;
  use ch4::pass::instruction_selection::Info as OldOldInfo;
  use ch4::pass::interference;
  use ch4::pass::liveness_analysis;
  use indexmap::indexmap;
  use insta::assert_snapshot;
  use maplit::hashmap;

  #[test]
  fn vector() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      pop rdi
      pop rsi
      push x
      mov w, rbx
      call rt_allocate, 2, gc
      add w, x
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Tuple(vec![Type::Int]),
          IdxVar::new("w") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{}", result.info));
  }

  #[test]
  fn str() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      pop rdi
      pop rsi
      push x
      mov w, rbx
      call rt_allocate, 2, gc
      add w, x
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexmap! {
          IdxVar::new("x") => Type::Str,
          IdxVar::new("w") => Type::Int,
        },
      },
      blocks,
      ..Program::default()
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = interference::build_interference(prog);

    assert_snapshot!(format!("{}", result.info));
  }
}
