use ast::{Program};
use std::collections::HashMap;
use support::CompileError;
use ch4::pass::typecheck::{Type, typecheck_exp};

pub type Result<T> = std::result::Result<T, CompileError>;

pub fn typecheck(prog: Program) -> Result<Program> {
  let body = prog
    .body
    .into_iter()
    .map(|exp| {
      let range = exp.0;
      let (exp, ty) = typecheck_exp(&mut HashMap::new(), exp)?;
      if ty != Type::Void {
        return Err(CompileError {
          range,
          message: format!("expected Void, found {:?}", ty),
        });
      }
      Ok(exp)
    })
    .collect::<Result<_>>()?;
  Ok(Program { body })
}