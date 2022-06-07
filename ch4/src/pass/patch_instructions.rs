#[cfg(test)]
mod tests {
  use super::super::*;
  use asm::{Label, Reg::*};
  use ast::*;
  use ch2::pass::remove_complex_operands;
  use ch3::location_set::LocationSet;
  use insta::assert_snapshot;
  use maplit::hashmap;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let prog = instruction_selection::select_instruction(prog, false);
    let prog = liveness_analysis::analyze_liveness(
      prog,
      hashmap! {
        Label::Epilogue => LocationSet::regs([Rax, Rbp])
      },
    );
    let prog = interference::build_interference(prog);
    let prog = move_biasing::build_move_graph(prog);
    let regs = &[
      Rbx, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, R12, R13, R14, R15,
    ];
    let prog = register_allocation::allocate_registers(prog, regs);
    let result = ch2::pass::patch_instructions::patch_instructions(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
