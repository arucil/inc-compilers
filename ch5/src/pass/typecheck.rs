use ast::{Exp, Program};
use ch4::pass::typecheck::{
  typecheck_if, typecheck_let, typecheck_prim, Result, Type,
};
use std::collections::HashMap;
use support::{CompileError, Range};

pub fn typecheck(prog: Program) -> Result<Program> {
  let body = prog
    .body
    .into_iter()
    .map(|exp| {
      let range = exp.0;
      let (exp, ty) = typecheck_exp(&mut HashMap::new(), exp)?;
      if ty != Type::Int {
        return Err(CompileError {
          range,
          message: format!("expected Int, found {:?}", ty),
        });
      }
      Ok(exp)
    })
    .collect::<Result<_>>()?;
  Ok(Program { body })
}

fn typecheck_exp(
  env: &mut HashMap<String, Type>,
  (range, exp): (Range, Exp),
) -> Result<((Range, Exp), Type)> {
  match exp {
    Exp::Int(_) => Ok(((range, exp), Type::Int)),
    Exp::Bool(_) => Ok(((range, exp), Type::Bool)),
    Exp::Str(_) => Ok(((range, exp), Type::Str)),
    Exp::Var(ref var) => {
      if let Some(&ty) = env.get(var) {
        Ok(((range, exp), ty))
      } else {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var),
        })
      }
    }
    Exp::Prim { op, args } => {
      typecheck_prim(range, op, args, env, typecheck_exp)
    }
    Exp::Let { var, init, body } => {
      typecheck_let(range, var, init, body, env, typecheck_exp)
    }
    Exp::If { cond, conseq, alt } => {
      typecheck_if(range, cond, conseq, alt, env, typecheck_exp)
    }
    Exp::Set { ref var, exp } => {
      if let Some(&ty) = env.get(&var.1) {
        let (exp, exp_ty) = typecheck_exp(env, *exp)?;
        if ty != exp_ty {
          return Err(CompileError {
            range,
            message: format!("type mismatch, {:?} != {:?}", ty, exp_ty),
          });
        }
        Ok((
          (
            range,
            Exp::Set {
              var: var.clone(),
              exp: box exp,
            },
          ),
          Type::Void,
        ))
      } else {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var.1),
        })
      }
    }
    Exp::Begin(mut seq) => {
      let (last, last_ty) = typecheck_exp(env, seq.pop().unwrap())?;
      let mut seq: Vec<_> = seq
        .into_iter()
        .map(|exp| typecheck_exp(env, exp).map(|r| r.0))
        .collect::<Result<_>>()?;
      seq.push(last);
      Ok(((range, Exp::Begin(seq)), last_ty))
    }
    Exp::While 
    Exp::Print(exp) => {
      let (exp, ty) = typecheck_exp(env, *exp)?;
      if let Type::Bool | Type::Int | Type::Str = ty {
        Ok(((range, Exp::Print(box exp)), Type::Void))
      } else {
        Err(CompileError {
          range,
          message: format!(
            "expected printable value, found value of type {:?}",
            ty
          ),
        })
      }
    }
    Exp::NewLine => Ok(((range, Exp::NewLine), Type::Void)),
    _ => {
      panic!("unimplemented {:?}", exp);
    }
  }
}
