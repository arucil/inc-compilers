use super::explicate_control::CInfo;
use asm::Program;
use ast::{IdxVar, Type};
use ch2::pass::instruction_selection::{CodeGen, Locals};
use control::*;
use indexmap::IndexMap;
use std::fmt::{self, Display, Formatter};

#[derive(Default)]
pub struct Info {
  pub locals: IndexMap<IdxVar, Type>,
}

impl Display for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}\n", self.locals)
  }
}

impl From<CInfo> for Info {
  fn from(info: CInfo) -> Self {
    Self {
      locals: info.locals,
    }
  }
}

impl Locals for Info {
  fn add_local(&mut self, local: IdxVar, ty: Type) {
    self.locals.insert(local, ty);
  }
}

pub fn select_instruction(
  prog: CProgram<CInfo>,
  use_heap: bool,
) -> Program<Info, IdxVar> {
  let types = prog.types;
  let mut codegen = CodeGen::new(&types, use_heap);
  let funs = prog
    .funs
    .into_iter()
    .map(|fun| codegen.gen_fun(fun))
    .collect();
  let blocks = codegen.gen_body(prog.body);
  let result = codegen.finish();
  Program {
    info: Info {
      locals: prog.info.locals,
    },
    blocks,
    constants: result.constants,
    externs: result.externs,
    funs,
    types,
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use ast::*;
  use ch2::pass::remove_complex_operands;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog, false);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn nested_if() {
    let prog = ast::parse(
      r#"
(let ([x (read)]
      [y (read)])
  (if (if (< x 1)
        (eq? x 0)
        (eq? x 2))
    (+ y 2)
    (+ y 10)))
    "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog, false);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn if_in_init() {
    let prog = ast::parse(
      r#"(let ([x (if (>= (read) 3) 10 77)]) (if (not (eq? x 10)) 41 2))"#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog, false);
    assert_snapshot!(result.to_string_pretty());
  }

  #[test]
  fn complex_if() {
    let prog = ast::parse(
      r#"
(let ([x (read)])
  (if (> x 100)
    (let ([y (read)])
      (if (or (and (>= x y) (< (- x y) 10))
              (and (< x y) (< (- y x) 10)))
        -1
        (- y)))
    (if (and (> x 40) (< x 60))
      5000
      x)))
      "#,
    )
    .unwrap();
    let prog = typecheck::typecheck(prog).unwrap();
    let prog = shrink::shrink(prog);
    let prog = uniquify::uniquify(prog);
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog, false);
    assert_snapshot!(result.to_string_pretty());
  }
}
