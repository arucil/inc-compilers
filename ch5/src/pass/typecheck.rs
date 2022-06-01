use ast::{Program, Type};
use ch4::pass::typecheck::TypeChecker;
use support::CompileError;

pub type Result<T> = std::result::Result<T, CompileError>;

pub fn typecheck(prog: Program) -> Result<Program<String, Type>> {
  let mut func_defs = prog.func_defs;
  let mut checker = TypeChecker::new(prog.type_defs, &mut func_defs)?;
  let func_defs = func_defs
    .into_iter()
    .map(|(name, func)| checker.typecheck_func(name, func))
    .collect::<Result<_>>()?;
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
    func_defs,
    type_defs: Default::default(),
    body,
    types: checker.types,
  })
}
