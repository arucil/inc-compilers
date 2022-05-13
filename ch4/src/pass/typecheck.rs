use ast::{Exp, ExpKind, Program, Type};
use id_arena::Arena;
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use support::{CompileError, Range};

pub type Result<T> = std::result::Result<T, CompileError>;

enum PrimType {
  Fixed { args: Vec<Type>, ret: Type },
  Overloaded(Vec<PrimType>),
  Vector,
  Eq,
  VecRef,
  VecSet,
  VecLen,
}

impl PrimType {
  fn new_binary(arg1: Type, arg2: Type, ret: Type) -> Self {
    Self::Fixed {
      args: vec![arg1, arg2],
      ret,
    }
  }

  fn new_unary(arg1: Type, ret: Type) -> Self {
    Self::Fixed {
      args: vec![arg1],
      ret,
    }
  }

  fn new_nullary(ret: Type) -> Self {
    Self::Fixed { args: vec![], ret }
  }
}

static PRIM_TYPES: Lazy<HashMap<&'static str, PrimType>> = Lazy::new(|| {
  use Type::*;
  hashmap! {
    "+" => PrimType::new_binary(Int, Int, Int),
    "-" => PrimType::Overloaded(vec![
      PrimType::new_unary(Int, Int),
      PrimType::new_binary(Int, Int, Int),
    ]),
    "read" => PrimType::new_nullary(Int),
    "and" => PrimType::new_binary(Bool, Bool, Bool),
    "or" => PrimType::new_binary(Bool, Bool, Bool),
    "not" => PrimType::new_unary(Bool, Bool),
    ">" => PrimType::Overloaded(vec![
      PrimType::new_binary(Int, Int, Bool),
      PrimType::new_binary(Str, Str, Bool)]),
    ">=" => PrimType::Overloaded(vec![
      PrimType::new_binary(Int, Int, Bool),
      PrimType::new_binary(Str, Str, Bool)]),
    "<" => PrimType::Overloaded(vec![
      PrimType::new_binary(Int, Int, Bool),
      PrimType::new_binary(Str, Str, Bool)]),
    "<=" => PrimType::Overloaded(vec![
      PrimType::new_binary(Int, Int, Bool),
      PrimType::new_binary(Str, Str, Bool)]),
    "eq?" => PrimType::Eq,
    "vector" => PrimType::Vector,
    "vector-ref" => PrimType::VecRef,
    "vector-set!" => PrimType::VecSet,
    "vector-length" => PrimType::VecLen,
  }
});

pub fn typecheck(prog: Program) -> Result<Program<String, Type>> {
  let mut checker = TypeChecker::new();
  let body = prog
    .body
    .into_iter()
    .map(|exp| {
      let range = exp.range;
      let exp = checker.typecheck(exp)?;
      if exp.ty != Type::Int {
        return Err(CompileError {
          range,
          message: format!("expected Int, found {:?}", exp.ty),
        });
      }
      Ok(exp)
    })
    .collect::<Result<_>>()?;
  Ok(Program {
    body,
    types: checker.types,
  })
}

#[derive(Debug, Clone, Default)]
pub struct TypeChecker {
  env: HashMap<String, Type>,
  pub types: Arena<Type>,
}

impl TypeChecker {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn typecheck(&mut self, exp: Exp) -> Result<Exp<String, Type>> {
    self.env.clear();
    self.typecheck_exp(exp)
  }

  fn typecheck_exp(&mut self, exp: Exp) -> Result<Exp<String, Type>> {
    let range = exp.range;
    match exp.kind {
      ExpKind::Int(n) => Ok(Exp {
        kind: ExpKind::Int(n),
        range,
        ty: Type::Int,
      }),
      ExpKind::Bool(b) => Ok(Exp {
        kind: ExpKind::Bool(b),
        range,
        ty: Type::Bool,
      }),
      ExpKind::Str(s) => Ok(Exp {
        kind: ExpKind::Str(s),
        range,
        ty: Type::Str,
      }),
      ExpKind::Void => Ok(Exp {
        kind: ExpKind::Void,
        range,
        ty: Type::Void,
      }),
      ExpKind::Var(var) => {
        if let Some(ty) = self.env.get(&var) {
          Ok(Exp {
            kind: ExpKind::Var(var),
            range,
            ty: ty.clone(),
          })
        } else {
          Err(CompileError {
            range,
            message: format!("variable {} not found", var),
          })
        }
      }
      ExpKind::Get(var) => {
        if let Some(ty) = self.env.get(&var) {
          Ok(Exp {
            kind: ExpKind::Get(var),
            range,
            ty: ty.clone(),
          })
        } else {
          Err(CompileError {
            range,
            message: format!("variable {} not found", var),
          })
        }
      }
      ExpKind::Prim { op, args } => {
        let args = args
          .into_iter()
          .map(|arg| self.typecheck_exp(arg))
          .collect::<Result<Vec<_>>>()?;
        let ty = if let Some(ty) = PRIM_TYPES.get(op.1) {
          self.typecheck_op(range, op.1, ty, &args)?
        } else {
          panic!("unimplemented op {}", op.1)
        };
        Ok(Exp {
          kind: ExpKind::Prim { op, args },
          range,
          ty,
        })
      }
      ExpKind::Let { var, init, body } => {
        let init = self.typecheck_exp(*init)?;
        let old_var_ty = self.env.insert(var.1.clone(), init.ty.clone());
        let body = self.typecheck_exp(*body)?;
        if let Some(old_var_ty) = old_var_ty {
          self.env.insert(var.1.clone(), old_var_ty);
        } else {
          self.env.remove(&var.1);
        }
        Ok(Exp {
          ty: body.ty.clone(),
          kind: ExpKind::Let {
            var,
            init: box init,
            body: box body,
          },
          range,
        })
      }
      ExpKind::If { cond, conseq, alt } => {
        let cond = self.typecheck_exp(*cond)?;
        if cond.ty != Type::Bool {
          return Err(CompileError {
            range: cond.range,
            message: format!("expected Bool, found {:?}", cond.ty),
          });
        }
        let conseq = self.typecheck_exp(*conseq)?;
        let alt = self.typecheck_exp(*alt)?;
        if conseq.ty != alt.ty {
          return Err(CompileError {
            range,
            message: format!("type mismatch, {:?} != {:?}", conseq.ty, alt.ty),
          });
        }
        Ok(Exp {
          ty: conseq.ty.clone(),
          kind: ExpKind::If {
            cond: box cond,
            conseq: box conseq,
            alt: box alt,
          },
          range,
        })
      }
      ExpKind::Set { ref var, exp } => {
        let exp = self.typecheck_exp(*exp)?;
        if let Some(ty) = self.env.get(&var.1) {
          if ty != &exp.ty {
            return Err(CompileError {
              range,
              message: format!("type mismatch, {:?} != {:?}", ty, exp.ty),
            });
          }
          Ok(Exp {
            kind: ExpKind::Set {
              var: var.clone(),
              exp: box exp,
            },
            range,
            ty: Type::Void,
          })
        } else {
          Err(CompileError {
            range,
            message: format!("variable {} not found", var.1),
          })
        }
      }
      ExpKind::Begin { seq, last } => {
        let last = self.typecheck_exp(*last)?;
        let seq: Vec<_> = seq
          .into_iter()
          .map(|exp| self.typecheck_exp(exp))
          .collect::<Result<_>>()?;
        Ok(Exp {
          ty: last.ty.clone(),
          kind: ExpKind::Begin {
            seq,
            last: box last,
          },
          range,
        })
      }
      ExpKind::While { cond, body } => {
        let cond = self.typecheck_exp(*cond)?;
        if cond.ty != Type::Bool {
          return Err(CompileError {
            range: cond.range,
            message: format!("expected Bool, found {:?}", cond.ty),
          });
        }
        let body = self.typecheck_exp(*body)?;
        Ok(Exp {
          kind: ExpKind::While {
            cond: box cond,
            body: box body,
          },
          range,
          ty: Type::Void,
        })
      }
      ExpKind::Print(args) => {
        let args = args
          .into_iter()
          .map(|exp| {
            let val = self.typecheck_exp(exp)?;
            if !matches!(val.ty, Type::Int | Type::Bool | Type::Str) {
              return Err(CompileError {
                range,
                message: format!(
                  "expected Int, Bool, or Str, found {:?}",
                  val.ty
                ),
              });
            }
            Ok(val)
          })
          .collect::<Result<Vec<_>>>()?;
        Ok(Exp {
          kind: ExpKind::Print(args),
          range,
          ty: Type::Void,
        })
      }
      ExpKind::NewLine => Ok(Exp {
        kind: ExpKind::NewLine,
        range,
        ty: Type::Void,
      }),
    }
  }

  fn typecheck_op(
    &mut self,
    range: Range,
    op: &str,
    ty: &PrimType,
    args: &[Exp<String, Type>],
  ) -> Result<Type> {
    match ty {
      PrimType::Fixed { args: arg_tys, ret } => {
        if arg_tys.len() != args.len() {
          return Err(CompileError {
            range,
            message: "arity mismatch".to_owned(),
          });
        }
        if args.iter().zip(arg_tys).all(|(x, y)| x.ty == *y) {
          Ok(ret.clone())
        } else {
          Err(CompileError {
            range,
            message: "type mismatch".to_owned(),
          })
        }
      }
      PrimType::Overloaded(alts) => {
        for alt in alts {
          if let Ok(ty) = self.typecheck_op(range, op, alt, args) {
            return Ok(ty);
          }
        }
        Err(CompileError {
          range,
          message: format!("invalid {} operation", op),
        })
      }
      PrimType::Eq => {
        if args.len() == 2 {
          if args[0].ty == args[1].ty {
            Ok(Type::Bool)
          } else {
            Err(CompileError {
              range,
              message: format!(
                "type mismatch, {:?} != {:?}",
                args[0].ty, args[1].ty
              ),
            })
          }
        } else {
          Err(CompileError {
            range,
            message: "arity mismatch".to_owned(),
          })
        }
      }
      PrimType::Vector => {
        Ok(Type::Vector(args.iter().map(|e| e.ty.clone()).collect()))
      }
      PrimType::VecLen => {
        if args.len() == 1 {
          if args[0].ty.to_vector(&self.types).is_some() {
            Ok(Type::Int)
          } else {
            Err(CompileError {
              range,
              message: "type mismatch".to_owned(),
            })
          }
        } else {
          Err(CompileError {
            range,
            message: "arity mismatch".to_owned(),
          })
        }
      }
      PrimType::VecRef => {
        if args.len() == 2 {
          if let Some(types) = args[0].ty.to_vector(&self.types) {
            if let ExpKind::Int(k) = &args[1].kind {
              if *k >= 0 && (*k as usize) < types.len() {
                Ok(types[*k as usize].clone())
              } else {
                Err(CompileError {
                  range,
                  message: "tuple index out of range".to_owned(),
                })
              }
            } else {
              Err(CompileError {
                range,
                message: "expected int literal".to_owned(),
              })
            }
          } else {
            Err(CompileError {
              range,
              message: "type mismatch".to_owned(),
            })
          }
        } else {
          Err(CompileError {
            range,
            message: "arity mismatch".to_owned(),
          })
        }
      }
      PrimType::VecSet => {
        if args.len() == 3 {
          if let Some(types) = args[0].ty.to_vector(&self.types) {
            if let ExpKind::Int(k) = &args[1].kind {
              if *k >= 0 && (*k as usize) < types.len() {
                if types[*k as usize] == args[2].ty {
                  Ok(Type::Void)
                } else {
                  Err(CompileError {
                    range,
                    message: "type mismatch".to_owned(),
                  })
                }
              } else {
                Err(CompileError {
                  range,
                  message: "tuple index out of range".to_owned(),
                })
              }
            } else {
              Err(CompileError {
                range,
                message: "expected int literal".to_owned(),
              })
            }
          } else {
            Err(CompileError {
              range,
              message: "type mismatch".to_owned(),
            })
          }
        } else {
          Err(CompileError {
            range,
            message: "arity mismatch".to_owned(),
          })
        }
      }
    }
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
        message: "type mismatch".to_owned(),
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
