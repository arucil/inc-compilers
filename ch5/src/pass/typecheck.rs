use ast::{Program, Type};
use ch4::pass::typecheck::typecheck_exp;
use std::collections::HashMap;
use support::CompileError;

pub type Result<T> = std::result::Result<T, CompileError>;

pub fn typecheck(prog: Program) -> Result<Program<String, Type>> {
  let body = prog
    .body
    .into_iter()
    .map(|exp| {
      let range = exp.range;
      let exp = typecheck_exp(&mut HashMap::new(), exp)?;
      if exp.ty != Type::Void {
        return Err(CompileError {
          range,
          message: format!("expected Void, found {:?}", exp.ty),
        });
      }
      Ok(exp)
    })
    .collect::<Result<_>>()?;
  Ok(Program { body })
}
