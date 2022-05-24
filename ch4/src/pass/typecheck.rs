use ast::{Exp, ExpKind, Program, StructDef, Type, TypeDef};
use id_arena::Arena;
use indexmap::IndexMap;
use maplit::hashmap;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use support::{CompileError, Range};

pub type Result<T> = std::result::Result<T, CompileError>;

enum PrimType {
  Fixed { args: Vec<Type>, ret: Type },
  Overloaded(Vec<PrimType>),
  MakeVec,
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
    "make-vector" => PrimType::MakeVec,
    "vector" => PrimType::Vector,
    "vector-ref" => PrimType::VecRef,
    "vector-set!" => PrimType::VecSet,
    "vector-length" => PrimType::VecLen,
  }
});

pub fn typecheck(prog: Program) -> Result<Program<String, Type>> {
  let mut checker = TypeChecker::new(&prog.defs)?;
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
    defs: prog.defs,
    body,
    types: checker.types,
  })
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
  env: HashMap<String, Type>,
  funcs: HashMap<String, FuncType>,
  pub types: Arena<Type>,
}

#[derive(Debug, Clone)]
struct FuncType {
  args: Vec<Type>,
  ret: RetType,
}

#[derive(Debug, Clone)]
enum RetType {
  Constructor(Type),
  Selector { index: usize, set: bool, ret: Type },
}

impl TypeChecker {
  pub fn new(defs: &IndexMap<String, StructDef>) -> Result<Self> {
    let mut types = Arena::new();
    let mut funcs = HashMap::new();
    let mut type_ids = HashMap::new();
    for name in defs.keys() {
      type_ids.insert(name, types.alloc(Type::Void));
    }
    for (name, StructDef(fields)) in defs {
      let id = type_ids[name];
      let fields = fields
        .iter()
        .enumerate()
        .map(|(index, (field_name, def))| {
          let ty = match def {
            TypeDef::Void => Type::Void,
            TypeDef::Bool => Type::Bool,
            TypeDef::Int => Type::Int,
            TypeDef::Str => Type::Str,
            TypeDef::Alias(range, alias) => {
              if let Some(id) = type_ids.get(alias) {
                Type::Alias(*id)
              } else {
                return Err(CompileError {
                  range: *range,
                  message: format!("type {} not found", alias),
                });
              }
            }
          };
          funcs.insert(
            format!("{}-{}", name, field_name),
            FuncType {
              args: vec![Type::Alias(id)],
              ret: RetType::Selector {
                index,
                set: false,
                ret: ty.clone(),
              },
            },
          );
          funcs.insert(
            format!("set-{}-{}!", name, field_name),
            FuncType {
              args: vec![Type::Alias(id), ty.clone()],
              ret: RetType::Selector {
                index,
                set: true,
                ret: Type::Void,
              },
            },
          );
          Ok(ty)
        })
        .collect::<Result<Vec<Type>>>()?;
      funcs.insert(
        name.clone(),
        FuncType {
          args: fields.clone(),
          ret: RetType::Constructor(Type::Alias(id)),
        },
      );
      *types.get_mut(id).unwrap() = Type::Tuple(fields);
    }
    Ok(Self {
      env: HashMap::new(),
      funcs,
      types,
    })
  }

  pub fn typecheck(&mut self, exp: Exp) -> Result<Exp<String, Type>> {
    self.env.clear();
    self.typecheck_exp(exp)
  }

  fn resolve_type(&self, ty: &Type) -> Type {
    ty.resolved(&self.types)
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
      ExpKind::Call { name, args } => {
        let mut args = args
          .into_iter()
          .map(|arg| self.typecheck_exp(arg))
          .collect::<Result<Vec<_>>>()?;
        let FuncType { args: arg_tys, ret } =
          self.funcs.get(&name.1).ok_or_else(|| CompileError {
            range: name.0,
            message: format!("function {} not found", name.1),
          })?;
        if args.len() != arg_tys.len() {
          return Err(CompileError {
            range: exp.range,
            message: format!("arity mismatch for {}", name.1),
          });
        }
        if !args.iter().zip(arg_tys).all(|(x, y)| x.ty == *y) {
          return Err(CompileError {
            range,
            message: format!("type mismatch for {}", name.1),
          });
        }
        match ret {
          RetType::Constructor(ty) => Ok(Exp {
            kind: ExpKind::Prim {
              op: (name.0, "vector"),
              args,
            },
            range,
            ty: ty.clone(),
          }),
          RetType::Selector { index, set, ret } => {
            args.insert(
              1,
              Exp {
                kind: ExpKind::Int(*index as i64),
                range: name.0,
                ty: Type::Int,
              },
            );
            Ok(Exp {
              kind: if *set {
                ExpKind::Prim {
                  op: (name.0, "vector-set!"),
                  args,
                }
              } else {
                ExpKind::Prim {
                  op: (name.0, "vector-ref"),
                  args,
                }
              },
              range,
              ty: ret.clone(),
            })
          }
        }
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
      PrimType::MakeVec => {
        if args.len() == 2 {
          if args[0].ty != Type::Int {
            Err(CompileError {
              range,
              message: format!(
                "expected Int for 1st argument of make-vector, found {:?}",
                args[0].ty
              ),
            })
          } else {
            Ok(Type::Array(box args[1].ty.clone()))
          }
        } else {
          Err(CompileError {
            range,
            message: "arity mismatch for make-vector".to_owned(),
          })
        }
      }
      PrimType::Vector => {
        Ok(Type::Tuple(args.iter().map(|e| e.ty.clone()).collect()))
      }
      PrimType::VecLen => {
        if args.len() == 1 {
          match self.resolve_type(&args[0].ty) {
            Type::Tuple(_) | Type::Array(_) => Ok(Type::Int),
            Type::Alias(_) => unreachable!(),
            _ => Err(CompileError {
              range,
              message: "type mismatch".to_owned(),
            }),
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
          match self.resolve_type(&args[0].ty) {
            Type::Tuple(fields) => {
              if let ExpKind::Int(k) = &args[1].kind {
                if *k >= 0 && (*k as usize) < fields.len() {
                  Ok(fields[*k as usize].clone())
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
            }
            Type::Array(ty) => {
              if self.resolve_type(&args[1].ty) == Type::Int {
                Ok(*ty)
              } else {
                Err(CompileError {
                  range,
                  message: "type mismatch for 2nd argument of vector-ref"
                    .to_owned(),
                })
              }
            }
            _ => Err(CompileError {
              range,
              message: "type mismatch for 1st argument of vector-ref"
                .to_owned(),
            }),
          }
        } else {
          Err(CompileError {
            range,
            message: "arity mismatch for vector-ref".to_owned(),
          })
        }
      }
      PrimType::VecSet => {
        if args.len() == 3 {
          match self.resolve_type(&args[0].ty) {
            Type::Tuple(fields) => {
              if let ExpKind::Int(k) = &args[1].kind {
                if *k >= 0 && (*k as usize) < fields.len() {
                  if fields[*k as usize] == args[2].ty {
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
            }
            Type::Array(ty) => {
              if self.resolve_type(&args[1].ty) != Type::Int {
                return Err(CompileError {
                  range,
                  message: "type mismatch for 2nd argument of vector-set!"
                    .to_owned(),
                });
              }
              if self.resolve_type(&args[2].ty) == self.resolve_type(&ty) {
                Ok(Type::Void)
              } else {
                Err(CompileError {
                  range,
                  message: "type mismatch for 3rd argument of vector-set!"
                    .to_owned(),
                })
              }
            }
            _ => Err(CompileError {
              range,
              message: "type mismatch".to_owned(),
            }),
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
