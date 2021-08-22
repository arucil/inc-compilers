use ast::{Exp, Program};
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use support::{CompileError, Range};

type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
  Int,
  Bool,
  Str,
}

static PRIM_TYPES: Lazy<HashMap<&'static str, Vec<(Vec<Type>, Type)>>> =
  Lazy::new(|| {
    hashmap! {
      "+" => vec![(vec![Type::Int, Type::Int], Type::Int)],
      "-" => vec![
        (vec![Type::Int], Type::Int),
        (vec![Type::Int, Type::Int], Type::Int)
      ],
      "read" => vec![(vec![], Type::Int)],
      "and" => vec![(vec![Type::Bool, Type::Bool], Type::Bool)],
      "or" => vec![(vec![Type::Bool, Type::Bool], Type::Bool)],
      "not" => vec![(vec![Type::Bool], Type::Bool)],
      ">" => vec![(vec![Type::Int, Type::Int], Type::Bool)],
      ">=" => vec![(vec![Type::Int, Type::Int], Type::Bool)],
      "<" => vec![(vec![Type::Int, Type::Int], Type::Bool)],
      "<=" => vec![(vec![Type::Int, Type::Int], Type::Bool)],
    }
  });

pub fn typecheck_prog(prog: Program) -> Result<Program> {
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
      let args = args
        .into_iter()
        .map(|arg| typecheck_exp(env, arg))
        .collect::<Result<Vec<_>>>()?;
      let arg_types = args.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
      let args = args.into_iter().map(|(arg, _)| arg).collect();
      let ty = typecheck_op(range.clone(), op.1, &arg_types)?;
      Ok(((range, Exp::Prim { op, args }), ty))
    }
    Exp::Let { var, init, body } => {
      let (init, init_ty) = typecheck_exp(env, *init)?;
      let old_var_ty = env.insert(var.1.clone(), init_ty);
      let (body, ty) = typecheck_exp(env, *body)?;
      if let Some(old_var_ty) = old_var_ty {
        env.insert(var.1.clone(), old_var_ty);
      } else {
        env.remove(&var.1);
      }
      Ok((
        (
          range,
          Exp::Let {
            var,
            init: box init,
            body: box body,
          },
        ),
        ty,
      ))
    }
    Exp::If { cond, conseq, alt } => {
      let (cond, ty) = typecheck_exp(env, *cond)?;
      if ty != Type::Bool {
        return Err(CompileError {
          range,
          message: format!("expected Bool, found {:?}", ty),
        });
      }
      let (conseq, ty1) = typecheck_exp(env, *conseq)?;
      let (alt, ty2) = typecheck_exp(env, *alt)?;
      if ty1 != ty2 {
        return Err(CompileError {
          range,
          message: format!("type mismatch in if form, {:?} != {:?}", ty1, ty2),
        });
      }
      Ok((
        (
          range,
          Exp::If {
            cond: box cond,
            conseq: box conseq,
            alt: box alt,
          },
        ),
        ty1,
      ))
    }
    _ => {
      panic!("unimplemented {:?}", exp);
    }
  }
}

fn typecheck_op(range: Range, op: &str, arg_types: &[Type]) -> Result<Type> {
  if op == "eq?" {
    if arg_types.len() == 2 {
      if arg_types[0] == arg_types[1] {
        Ok(Type::Bool)
      } else {
        Err(CompileError {
          range,
          message: format!(
            "type mistmatch in eq? form, {:?} != {:?}",
            arg_types[0], arg_types[1]
          ),
        })
      }
    } else {
      Err(CompileError {
        range,
        message: format!("arity mismatch in eq?"),
      })
    }
  } else if let Some(tys) = PRIM_TYPES.get(&op) {
    for (arg_tys, ret_ty) in tys {
      if arg_types.len() == arg_tys.len()
        && arg_tys
          .iter()
          .zip(arg_types.iter())
          .map(|(x, y)| x == y)
          .all(|x| x)
      {
        return Ok(*ret_ty);
      }
    }
    Err(CompileError {
      range,
      message: format!("invalid {:?} operation", op),
    })
  } else {
    panic!("unimplemented op {:?}", op)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn nested_prims() {
    let prog = parse(r#"(+ 1 (+ (read) (- 7)))"#).unwrap();
    let result = typecheck_prog(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn r#if() {
    let prog = parse(r#"(if (> (read) (- 2 7)) (- 7) (+ (read) 30))"#).unwrap();
    let result = typecheck_prog(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn logical() {
    let prog =
      parse(r#"(if (and (or #f (>= (- 12) (read))) (not (<= 7 2))) 1 2)"#).unwrap();
    let result = typecheck_prog(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn r#let() {
    let prog =
      parse(r#"(let ([a (+ 7 2)]) (if (< a (read)) 13 (+ a 10)))"#).unwrap();
    let result = typecheck_prog(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }
}
