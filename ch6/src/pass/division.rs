use ast::{Error, Exp, ExpKind, Program, Type};
use id_arena::Arena;
use support::Range;

pub fn insert_division_check(
  prog: Program<String, Type>,
) -> Program<String, Type> {
  let types = &prog.types;
  Program {
    body: prog
      .body
      .into_iter()
      .map(|exp| exp_insert(exp, types))
      .collect(),
    ..prog
  }
}