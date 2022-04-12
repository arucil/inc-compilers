use ast::{Exp, PrintType, Program};
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use support::{CompileError, Range};

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
  Int,
  Bool,
  Str,
  Void,
}

static PRIM_TYPES: Lazy<HashMap<&'static str, Vec<(Vec<Type>, Type)>>> =
  Lazy::new(|| {
    use Type::*;
    hashmap! {
      "+" => vec![(vec![Int, Int], Int)],
      "-" => vec![
        (vec![Int], Int),
        (vec![Int, Int], Int)
      ],
      "read" => vec![(vec![], Int)],
      "and" => vec![(vec![Bool, Bool], Bool)],
      "or" => vec![(vec![Bool, Bool], Bool)],
      "not" => vec![(vec![Bool], Bool)],
      ">" => vec![(vec![Int, Int], Bool)],
      ">=" => vec![(vec![Int, Int], Bool)],
      "<" => vec![(vec![Int, Int], Bool)],
      "<=" => vec![(vec![Int, Int], Bool)],
    }
  });

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
      if !matches!(init_ty, Type::Int | Type::Bool) {
        return Err(CompileError {
          range: init.0,
          message: format!(
            "variable can only be Int or Bool, found {:?}",
            init_ty
          ),
        });
      }
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
          range: cond.0,
          message: format!("expected Bool, found {:?}", ty),
        });
      }
      let (conseq, ty1) = typecheck_exp(env, *conseq)?;
      let (alt, ty2) = typecheck_exp(env, *alt)?;
      if ty1 != ty2 {
        return Err(CompileError {
          range,
          message: format!("type mismatch, {:?} != {:?}", ty1, ty2),
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
    Exp::Begin { seq, last } => {
      let (last, last_ty) = typecheck_exp(env, *last)?;
      let seq: Vec<_> = seq
        .into_iter()
        .map(|exp| typecheck_exp(env, exp).map(|r| r.0))
        .collect::<Result<_>>()?;
      Ok((
        (
          range,
          Exp::Begin {
            seq,
            last: box last,
          },
        ),
        last_ty,
      ))
    }
    Exp::While { cond, body } => {
      let (cond, ty) = typecheck_exp(env, *cond)?;
      if ty != Type::Bool {
        return Err(CompileError {
          range: cond.0,
          message: format!("expected Bool, found {:?}", ty),
        });
      }
      let (body, _) = typecheck_exp(env, *body)?;
      Ok((
        (
          range,
          Exp::While {
            cond: box cond,
            body: box body,
          },
        ),
        Type::Void,
      ))
    }
    Exp::Print { val, ty: _ } => {
      let (val, ty) = typecheck_exp(env, *val)?;
      let ty = match ty {
        Type::Int => PrintType::Int,
        Type::Bool => PrintType::Bool,
        Type::Str => PrintType::Str,
        _ => {
          return Err(CompileError {
            range,
            message: format!("expected Int, Bool, or Str, found {:?}", ty),
          });
        }
      };
      Ok(((range, Exp::Print { val: box val, ty }), Type::Void))
    }
    Exp::NewLine => Ok(((range, Exp::NewLine), Type::Void)),
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
            "type mismatch, {:?} != {:?}",
            arg_types[0], arg_types[1]
          ),
        })
      }
    } else {
      Err(CompileError {
        range,
        message: format!("arity mismatch"),
      })
    }
  } else if let Some(tys) = PRIM_TYPES.get(&op) {
    let mut arity_matched = false;
    let mut matched_arg_tys = Vec::<&[Type]>::new();
    for (arg_tys, ret_ty) in tys {
      if arg_types.len() == arg_tys.len() {
        arity_matched = true;
        if arg_tys.iter().zip(arg_types.iter()).all(|(x, y)| x == y) {
          return Ok(*ret_ty);
        }
      } else {
        matched_arg_tys.push(arg_tys);
      }
    }
    if arity_matched {
      Err(CompileError {
        range,
        message: format!("invalid {:?} operation", op),
      })
    } else {
      Err(CompileError {
        range,
        message: format!("arity mismatch"),
      })
    }
  } else {
    panic!("unimplemented op {:?}", op)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use pretty_assertions::assert_eq;
  use support::Range;

  #[test]
  fn nested_prims() {
    let prog = parse(r#"(+ 1 (+ (read) (- 7)))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn r#if() {
    let prog = parse(r#"(if (> (read) (- 2 7)) (- 7) (+ (read) 30))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn logical() {
    let prog =
      parse(r#"(if (and (or #f (>= (- 12) (read))) (not (<= 7 2))) 1 2)"#)
        .unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn eq() {
    let prog = parse(r#"(if (eq? #t (not (eq? 3 (read)))) 1 2)"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn r#let() {
    let prog =
      parse(r#"(let ([a (+ 7 2)]) (if (< a (read)) 13 (+ a 10)))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn if_mismatch() {
    let prog = parse(r#"(if #t (+ 7 3) (not (> 3 2)))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result,
      Err(CompileError {
        range: Range { start: 0, end: 29 },
        message: format!("type mismatch, Int != Bool"),
      })
    );
  }

  #[test]
  fn if_cond_not_bool() {
    let prog = parse(r#"(if (read) (+ 7 3) (- (> 3 2)))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result,
      Err(CompileError {
        range: Range { start: 4, end: 10 },
        message: format!("expected Bool, found Int"),
      })
    );
  }

  #[test]
  fn var_not_found() {
    let prog = parse(r#"x1"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result,
      Err(CompileError {
        range: Range { start: 0, end: 2 },
        message: format!("variable x1 not found"),
      })
    );
  }

  #[test]
  fn eq_type_mismatch() {
    let prog = parse(r#"(eq? #t (+ 3 7))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result,
      Err(CompileError {
        range: Range { start: 0, end: 16 },
        message: format!("type mismatch, Bool != Int"),
      })
    );
  }

  #[test]
  fn arity_mismatch() {
    let prog = parse(r#"(eq? 3 (+ 3 7 2))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result,
      Err(CompileError {
        range: Range { start: 7, end: 16 },
        message: format!("arity mismatch"),
      })
    );
  }

  #[test]
  fn type_mismatch_in_addition() {
    let prog = parse(r#"(+ 3 (not #t))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result,
      Err(CompileError {
        range: Range { start: 0, end: 14 },
        message: format!("invalid \"+\" operation"),
      })
    );
  }

  #[test]
  fn set() {
    let prog = parse(r#"(let ([x 1]) (set! x (+ x 1)) x)"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }

  #[test]
  fn set_returns_void() {
    let prog = parse(r#"(let ([x 1]) (set! x (+ x 1)))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 0, end: 30 },
        message: format!("expected Int, found Void"),
      })
    );
  }

  #[test]
  fn begin_returns_last_type() {
    let prog = parse(r#"(begin (- 7) (begin (+ 2 3) (> 2 1)))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 0, end: 37 },
        message: format!("expected Int, found Bool"),
      })
    );
  }

  #[test]
  fn while_loop() {
    let prog =
      parse(r#"(let ([x 1]) (while (< x 3) (set! x (+ x 1)) "x"))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 0, end: 50 },
        message: format!("expected Int, found Void"),
      })
    );
  }

  #[test]
  fn while_cond_mismatch() {
    let prog =
      parse(r#"(let ([x 1]) (while (+ x 3) (set! x (+ x 1)) "x"))"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(
      result.map(|_| ()),
      Err(CompileError {
        range: Range { start: 20, end: 27 },
        message: format!("expected Bool, found Int"),
      })
    );
  }

  #[test]
  fn print() {
    let prog = parse(r#"(begin (print (+ 1 2) "abc" #f) 0)"#).unwrap();
    let result = typecheck(prog);
    assert_eq!(result.map(|_| ()), Ok(()));
  }
}
