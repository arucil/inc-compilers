use ast::{Program, Type};
use ch4::pass::typecheck::TypeChecker;
use support::CompileError;

pub type Result<T> = std::result::Result<T, CompileError>;

pub fn typecheck(prog: Program) -> Result<Program<String, Type>> {
  let mut checker = TypeChecker::new(prog.defs)?;
  let last = prog.body.len() - 1;
  let body = prog
    .body
    .into_iter()
    .enumerate()
    .map(|(i, exp)| {
      let range = exp.range;
      let exp = checker.typecheck(exp)?;
      if i == last && exp.ty != Type::Void {
        return Err(CompileError {
          range,
          message: format!("expected Void, found {:?}", exp.ty),
        });
      }
      Ok(exp)
    })
    .collect::<Result<_>>()?;
  Ok(Program {
    defs: Default::default(),
    body,
    types: checker.types,
  })
}
