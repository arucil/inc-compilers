use ast::{Exp, Program, Type};
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use support::{CompileError, Range};

pub type Result<T> = std::result::Result<T, CompileError>;

type PrimType = Vec<(Vec<Type>, Type)>;

static PRIM_TYPES: Lazy<HashMap<&'static str, PrimType>> = Lazy::new(|| {
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
    // TODO compare strings
    ">" => vec![(vec![Int, Int], Bool)],
    ">=" => vec![(vec![Int, Int], Bool)],
    "<" => vec![(vec![Int, Int], Bool)],
    "<=" => vec![(vec![Int, Int], Bool)],
  }
});

pub fn typecheck(prog: Program) -> Result<Program<String>> {
  let body = prog
    .body
    .into_iter()
    .map(|exp| {
      let range = exp.range();
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
  Ok(Program {
    body,
    types: prog.types,
  })
}

pub fn typecheck_exp(
  env: &mut HashMap<String, Type>,
  exp: Exp,
) -> Result<(Exp<String>, Type)> {
  match exp {
    Exp::Int { .. } => Ok((exp, Type::Int)),
    Exp::Bool { .. } => Ok((exp, Type::Bool)),
    Exp::Str { .. } => Ok((exp, Type::Str)),
    Exp::Void(_) => Ok((exp, Type::Void)),
    Exp::Var { var, range } => {
      if let Some(&ty) = env.get(&var) {
        Ok((Exp::Var { var, range }, ty))
      } else {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var),
        })
      }
    }
    Exp::Get { var, range } => {
      if let Some(&ty) = env.get(&var) {
        Ok((Exp::Get { var, range }, ty))
      } else {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var),
        })
      }
    }
    Exp::Prim { op, args, range } => {
      let args = args
        .into_iter()
        .map(|arg| typecheck_exp(env, arg))
        .collect::<Result<Vec<_>>>()?;
      let (args, tys): (_, Vec<_>) = args.into_iter().unzip();
      let ty = typecheck_op(range, op.1, &tys)?;
      // TODO vector return HasType
      Ok((Exp::Prim { op, args, range }, ty))
    }
    Exp::Let {
      var,
      init,
      body,
      range,
    } => {
      let (init, init_ty) = typecheck_exp(env, *init)?;
      let old_var_ty = env.insert(var.1.clone(), init_ty);
      let (body, body_ty) = typecheck_exp(env, *body)?;
      if let Some(old_var_ty) = old_var_ty {
        env.insert(var.1.clone(), old_var_ty);
      } else {
        env.remove(&var.1);
      }
      Ok((
        Exp::Let {
          var,
          init: box Exp::HasType {
            exp: box init,
            ty: init_ty,
          },
          body: box Exp::HasType {
            exp: box body,
            ty: body_ty,
          },
          range,
        },
        body_ty,
      ))
    }
    Exp::If {
      cond,
      conseq,
      alt,
      range,
    } => {
      let cond_range = cond.range();
      let (cond, cond_ty) = typecheck_exp(env, *cond)?;
      if cond_ty != Type::Bool {
        return Err(CompileError {
          range: cond_range,
          message: format!("expected Bool, found {:?}", cond_ty),
        });
      }
      let (conseq, conseq_ty) = typecheck_exp(env, *conseq)?;
      let (alt, alt_ty) = typecheck_exp(env, *alt)?;
      if conseq_ty != alt_ty {
        return Err(CompileError {
          range,
          message: format!("type mismatch, {:?} != {:?}", conseq_ty, alt_ty),
        });
      }
      Ok((
        Exp::If {
          cond: box cond,
          conseq: box conseq,
          alt: box alt,
          range,
        },
        conseq_ty,
      ))
    }
    Exp::Set { var, exp, range } => {
      if let Some(&ty) = env.get(&var.1) {
        let (exp, exp_ty) = typecheck_exp(env, *exp)?;
        if ty != exp_ty {
          return Err(CompileError {
            range,
            message: format!("type mismatch, {:?} != {:?}", ty, exp_ty),
          });
        }
        Ok((
          Exp::Set {
            var: var.clone(),
            exp: box exp,
            range,
          },
          Type::Void,
        ))
      } else {
        Err(CompileError {
          range,
          message: format!("variable {} not found", var.1),
        })
      }
    }
    Exp::Begin { seq, last, range } => {
      let (last, last_ty) = typecheck_exp(env, *last)?;
      let seq: Vec<_> = seq
        .into_iter()
        .map(|exp| Ok(typecheck_exp(env, exp)?.0))
        .collect::<Result<_>>()?;
      Ok((
        Exp::Begin {
          seq,
          last: box last,
          range,
        },
        last_ty,
      ))
    }
    Exp::While { cond, body, range } => {
      let cond_range = cond.range();
      let (cond, cond_ty) = typecheck_exp(env, *cond)?;
      if cond_ty != Type::Bool {
        return Err(CompileError {
          range: cond_range,
          message: format!("expected Bool, found {:?}", cond_ty),
        });
      }
      let (body, _) = typecheck_exp(env, *body)?;
      Ok((
        Exp::While {
          cond: box cond,
          body: box body,
          range,
        },
        Type::Void,
      ))
    }
    Exp::Print { args, range } => {
      let args = args
        .into_iter()
        .map(|exp| {
          let (exp, exp_ty) = typecheck_exp(env, exp)?;
          if !matches!(exp_ty, Type::Int | Type::Bool | Type::Str) {
            return Err(CompileError {
              range,
              message: format!(
                "expected Int, Bool, or Str, found {:?}",
                exp_ty
              ),
            });
          }
          Ok(Exp::HasType {
            exp: box exp,
            ty: exp_ty,
          })
        })
        .collect::<Result<Vec<_>>>()?;
      Ok((Exp::Print { args, range }, Type::Void))
    }
    Exp::NewLine(range) => Ok((Exp::NewLine(range), Type::Void)),
    Exp::HasType { .. } => unreachable!(),
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
        message: "arity mismatch".to_owned(),
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
        message: "arity mismatch".to_owned(),
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
        message: "type mismatch, Int != Bool".to_owned(),
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
        message: "expected Bool, found Int".to_owned(),
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
        message: "variable x1 not found".to_owned(),
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
        message: "type mismatch, Bool != Int".to_owned(),
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
        message: "arity mismatch".to_owned(),
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
        message: "invalid \"+\" operation".to_owned(),
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
        message: "expected Int, found Void".to_owned(),
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
        message: "expected Int, found Bool".to_owned(),
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
        message: "expected Int, found Void".to_owned(),
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
        message: "expected Bool, found Int".to_owned(),
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
