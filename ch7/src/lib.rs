#![allow(incomplete_features)]
#![feature(box_syntax, type_changing_struct_update)]

use asm::{Label, Reg};
use ch3::location_set::LocationSet;
use maplit::hashmap;
use support::CompileError;

pub mod pass;

pub fn compile(
  input: &str,
  regs: Option<&[Reg]>,
) -> Result<String, CompileError> {
  use Reg::*;
  let prog = ast::parse(input)?;
  let prog = ch5::pass::typecheck::typecheck(prog)?;
  let prog = ch6::pass::r#struct::desugar_struct(prog);
  let prog = ch4::pass::shrink::shrink(prog);
  let prog = ch4::pass::uniquify::uniquify(prog);
  let prog = ch5::pass::uncover_get::uncover_get(prog);
  let prog = ch6::pass::array_bounds::insert_bounds_check(prog);
  let prog = ch6::pass::division::insert_division_check(prog);
  let prog = ch6::pass::string::expose_string_concat(prog);
  let prog = self::pass::parameter::limit_arity(prog);
  let prog = ch2::pass::remove_complex_operands::remove_complex_operands(prog);
  let prog = ch4::pass::explicate_control::explicate_control(prog);
  let prog = ch4::pass::instruction_selection::select_instruction(prog, true);
  let prog = self::pass::liveness_analysis::analyze_liveness(
    prog,
    hashmap! {
      Label::Epilogue => LocationSet::regs([Rbp, R15])
    },
    hashmap! {
      Label::Epilogue => LocationSet::regs([Rbp, R15])
    },
  );
  let prog = ch4::pass::interference::build_interference(prog);
  let prog = ch4::pass::move_biasing::build_move_graph(prog);
  let regs =
    regs.unwrap_or(&[Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11, Rbx, R12, R13, R14]);
  let prog = ch6::pass::register_allocation::allocate_registers(prog, regs);
  let prog = ch2::pass::patch_instructions::patch_instructions(prog);
  let prog = self::pass::tailcall::patch_tailjmps(prog);
  let prog = ch6::pass::perilogue::add_perilogue(prog);
  let prog = ch4::pass::merge_blocks::merge_blocks(prog);

  Ok(prog.to_nasm(false))
}

#[cfg(test)]
mod tests {
  use super::*;
  use insta::assert_snapshot;

  #[test]
  fn example_in_book() {
    let prog = compile(
      r#"
(define (add [x Int] [y Int]) : Int
  (+ x y))
(define (main) : Int
  (add 4 2))
(print (main))
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn pass_functions() {
    let prog = compile(
      r#"
(define (do [f ( -> Int)] [g (Int -> Str)]) : Str
  (g (f)))
(define (foo) : Int
  1)
(define (bar [x Int]) : Str
  (print x)
  "abc")
(do foo bar)
(void)
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn return_functions() {
    let prog = compile(
      r#"
(define (do [x Int]) : (Int -> Int)
  (if (> x 2)
    foo
    (bar x)))
(define (foo [x Int]) : Int
  (+ x 1))
(define (bar [x Int]) : (Int -> Int)
  (print x)
  foo)
(print ((do 3) 7))
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn recursion() {
    let prog = compile(
      r#"
(define (fact1 [x Int]) : Int
  (if (eq? x 0)
    1
    (* x (fact1 (- x 1)))))
(define (fact2 [x Int] [y Int]) : Int
  (if (eq? x 0)
    y
    (fact2 (- x 1) (* y x))))
(fact1 7)
(fact2 7 1)
(void)
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn mul_div() {
    let prog = compile(
      r#"
(print (* 7 (read)))
(print (* 2147483648 (read)))
(print (* (read) -11))
(print (* (read) -2147483649))
(let ([x 1] [y 2])
  (print (remainder (quotient (* x y) 30) -12) (quotient 3 x) (remainder x (- y))))
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }

  #[test]
  fn call_arg_reg() {
    let prog = compile(
      r#"
(define (foo [x Int] [y Int] [z Int] [w Int]) : Void
  (void))
(let ([a 1] [x (+ a 1)] [y (+ x 2)] [z (+ a 3)] [w (+ z 4)])
  (foo x y z w)
  (foo z x y w))
      "#,
      None,
    )
    .unwrap();
    assert_snapshot!(prog);
  }
}
