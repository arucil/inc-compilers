use ast::{Program, Type};
use ch4::pass::typecheck::TypeChecker;
use support::CompileError;

pub type Result<T> = std::result::Result<T, CompileError>;

pub fn typecheck(prog: Program) -> Result<Program<String, Type>> {
  let mut fun_defs = prog.fun_defs;
  let mut checker = TypeChecker::new(prog.type_defs, &mut fun_defs)?;
  let fun_defs = fun_defs
    .into_iter()
    .map(|(name, fun)| checker.typecheck_fun(name, fun))
    .collect::<Result<_>>()?;
  let range = prog.body.range;
  let body = checker.typecheck(prog.body)?;
  if body.ty != Type::Void {
    return Err(CompileError {
      range,
      message: format!("expected Void, found {:?}", body.ty),
    });
  }
  Ok(Program {
    fun_defs,
    type_defs: Default::default(),
    body,
    types: checker.types,
  })
}
