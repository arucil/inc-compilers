use ast::{Error, Exp, ExpKind, FuncDef, Program, StructApp, Type};
use id_arena::Arena;

pub fn desugar_struct(
  mut prog: Program<String, Type>,
) -> Program<String, Type> {
  let func_defs = prog
    .func_defs
    .into_iter()
    .map(|(name, func)| {
      (
        name,
        FuncDef {
          body: exp_desugar(func.body, &prog.types),
          ..func
        },
      )
    })
    .collect();
  let body = prog
    .body
    .into_iter()
    .map(|exp| exp_desugar(exp, &prog.types))
    .collect();
  for (_, ty) in &mut prog.types {
    if let Type::Struct(fields) = std::mem::replace(ty, Type::Void) {
      *ty = Type::Tuple(fields.into_values().collect())
    }
  }
  Program {
    func_defs,
    body,
    ..prog
  }
}

/// (my-struct-field x) => (vector-ref x K)
///
/// (set-my-struct-field! x v) => (vector-set! x K v)
///
/// (my-struct v1 v2 ...) => (vector v1 v2 ...)
fn exp_desugar(
  exp: Exp<String, Type>,
  types: &Arena<Type>,
) -> Exp<String, Type> {
  match exp.kind {
    ExpKind::Int(_)
    | ExpKind::Var(_)
    | ExpKind::Str(_)
    | ExpKind::Bool(_)
    | ExpKind::Get(_)
    | ExpKind::Void
    | ExpKind::NewLine => exp,
    ExpKind::Prim { op, args } => Exp {
      kind: ExpKind::Prim {
        op,
        args: args
          .into_iter()
          .map(|exp| exp_desugar(exp, types))
          .collect(),
      },
      ..exp
    },
    ExpKind::Apply {
      func,
      args,
      r#struct,
    } => {
      let func = exp_desugar(*func, types);
      let mut args = args
        .into_iter()
        .map(|exp| exp_desugar(exp, types))
        .collect();
      if let Some(app) = r#struct {
        match app {
          StructApp::Ctor => Exp {
            kind: ExpKind::Prim {
              op: (func.range, "vector"),
              args,
            },
            ..exp
          },
          StructApp::Getter(index) => {
            args.push(Exp {
              kind: ExpKind::Int(index as i64),
              range: func.range,
              ty: Type::Int,
            });
            Exp {
              kind: ExpKind::Prim {
                op: (func.range, "vector-ref"),
                args,
              },
              ..exp
            }
          }
          StructApp::Setter(index) => {
            args.insert(
              1,
              Exp {
                kind: ExpKind::Int(index as i64),
                range: func.range,
                ty: Type::Int,
              },
            );
            Exp {
              kind: ExpKind::Prim {
                op: (func.range, "vector-set!"),
                args,
              },
              ..exp
            }
          }
        }
      } else {
        Exp {
          kind: ExpKind::Apply {
            func: box func,
            args,
            r#struct: None,
          },
          ..exp
        }
      }
    }
    ExpKind::Let { var, init, body } => Exp {
      kind: ExpKind::Let {
        var,
        init: box exp_desugar(*init, types),
        body: box exp_desugar(*body, types),
      },
      ..exp
    },
    ExpKind::If { cond, conseq, alt } => Exp {
      kind: ExpKind::If {
        cond: box exp_desugar(*cond, types),
        conseq: box exp_desugar(*conseq, types),
        alt: box exp_desugar(*alt, types),
      },
      ..exp
    },
    ExpKind::Set { var, exp: exp1 } => Exp {
      kind: ExpKind::Set {
        var,
        exp: box exp_desugar(*exp1, types),
      },
      ..exp
    },
    ExpKind::Begin { seq, last } => Exp {
      kind: ExpKind::Begin {
        seq: seq.into_iter().map(|exp| exp_desugar(exp, types)).collect(),
        last: box exp_desugar(*last, types),
      },
      ..exp
    },
    ExpKind::While { cond, body } => Exp {
      kind: ExpKind::While {
        cond: box exp_desugar(*cond, types),
        body: box exp_desugar(*body, types),
      },
      ..exp
    },
    ExpKind::Print(args) => Exp {
      kind: ExpKind::Print(
        args
          .into_iter()
          .map(|exp| exp_desugar(exp, types))
          .collect(),
      ),
      ..exp
    },
    ExpKind::Error(Error::Length(arg)) => Exp {
      kind: ExpKind::Error(Error::Length(box exp_desugar(*arg, types))),
      ..exp
    },
    ExpKind::Error(Error::OutOfBounds { index, len }) => Exp {
      kind: ExpKind::Error(Error::OutOfBounds {
        index: box exp_desugar(*index, types),
        len: box exp_desugar(*len, types),
      }),
      ..exp
    },
    ExpKind::Error(Error::DivByZero) => exp,
    ExpKind::FunRef { .. } => exp,
  }
}
