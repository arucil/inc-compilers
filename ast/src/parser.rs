use std::cmp::Ordering;

use crate::{Exp, ExpKind, FunDef, Program, Type, TypeAlias};
use indexmap::IndexMap;
use std::collections::HashSet;
use support::{CompileError, Range};

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Cst {
  List(Vec<Cst>, Range),
  Symbol(Range),
  String(Range),
  Number(Range),
  Boolean(bool, Range),
}

impl Cst {
  fn range(&self) -> Range {
    match self {
      Self::List(_, range)
      | Self::Symbol(range)
      | Self::String(range)
      | Self::Number(range)
      | Self::Boolean(_, range) => *range,
    }
  }
}

struct Parser<'a> {
  input: &'a str,
  s: &'a str,
  token: (Range, Token),
  i: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Token {
  Eof,
  SymOrNum,
  Punc(char),
  Str,
  Bool(bool),
}

impl<'a> Parser<'a> {
  fn new(input: &'a str) -> Self {
    Self {
      input,
      s: input,
      token: ((0, 0).into(), Token::Eof),
      i: 0,
    }
  }

  fn peek(&mut self) -> Option<char> {
    return self.s.chars().next();
  }

  fn get_token(&mut self) -> Result<Token> {
    let mut c = self.peek();
    while matches!(c, Some(c) if c.is_whitespace() || c == ';') {
      if c == Some(';') {
        self.s = self.s.trim_start_matches(|c: char| c != '\n');
        self.i = self.input.len() - self.s.len();
      } else {
        self.advance();
      }
      c = self.peek();
    }

    let start = self.i;
    match self.peek() {
      Some(c @ ('(' | ')' | '[' | ']')) => {
        self.advance();
        let tok = Token::Punc(c);
        self.token = ((start, self.i).into(), Token::Punc(c));
        Ok(tok)
      }
      Some('"') => {
        self.advance();
        while self.peek() != Some('"') {
          match self.peek() {
            Some('\\') => self.advance(),
            None => {
              return Err(CompileError {
                range: (start, self.i).into(),
                message: "unclosed string".to_owned(),
              })
            }
            _ => {}
          }
          self.advance();
        }
        self.advance();
        self.token = ((start, self.i).into(), Token::Str);
        Ok(Token::Str)
      }
      Some('#') => {
        self.advance();
        while self.peek().map_or(false, |c| c.is_ascii_alphanumeric()) {
          self.advance();
        }
        let kind = match &self.input[start..self.i] {
          "#t" => Token::Bool(true),
          "#f" => Token::Bool(false),
          _ => {
            return Err(CompileError {
              range: (start, self.i).into(),
              message: "unrecognized hashtag".to_owned(),
            })
          }
        };
        self.token = ((start, self.i).into(), kind);
        Ok(kind)
      }
      Some(c) if !"])".contains(c) => {
        self.advance();
        let mut c = self.peek();
        while matches!(c, Some(c) if !c.is_whitespace() && !"()[];'\"".contains(c))
        {
          self.advance();
          c = self.peek();
        }
        self.token = ((start, self.i).into(), Token::SymOrNum);
        Ok(Token::SymOrNum)
      }
      None => {
        self.token = ((start, self.i).into(), Token::Eof);
        Ok(Token::Eof)
      }
      Some(c) => Err(CompileError {
        range: (start, start + c.len_utf8()).into(),
        message: format!("unexpected char '{}'", c),
      }),
    }
  }

  fn advance(&mut self) {
    let len = self.s.chars().next().unwrap().len_utf8();
    self.i += len;
    self.s = &self.s[len..];
  }

  fn parse(&mut self) -> Result<Vec<Cst>> {
    self.get_token()?;
    let mut xs = vec![];
    while self.token.1 != Token::Eof {
      xs.push(self.parse_exp()?);
    }
    Ok(xs)
  }

  fn parse_exp(&mut self) -> Result<Cst> {
    match self.token.1 {
      Token::Punc('(') => self.parse_list(')'),
      Token::Punc('[') => self.parse_list(']'),
      Token::Str => {
        let range = self.token.0;
        self.get_token()?;
        Ok(Cst::String(range))
      }
      Token::SymOrNum => self.parse_sym(self.token.0),
      Token::Eof => Err(CompileError {
        range: self.token.0,
        message: "unexpected EOF".to_owned(),
      }),
      Token::Punc(c) => Err(CompileError {
        range: self.token.0,
        message: format!("unexpected char '{}'", c),
      }),
      Token::Bool(b) => {
        let range = self.token.0;
        self.get_token()?;
        Ok(Cst::Boolean(b, range))
      }
    }
  }

  fn parse_sym(&mut self, range: Range) -> Result<Cst> {
    self.get_token()?;
    if let Err(err) = self.input[range.start..range.end].parse::<i64>() {
      use std::num::IntErrorKind;
      if let IntErrorKind::PosOverflow | IntErrorKind::NegOverflow = err.kind()
      {
        Err(CompileError {
          range,
          message: "integer overflow".to_owned(),
        })
      } else {
        Ok(Cst::Symbol(range))
      }
    } else {
      Ok(Cst::Number(range))
    }
  }

  fn parse_list(&mut self, close: char) -> Result<Cst> {
    let start = self.token.0.start;
    self.get_token()?;
    let mut xs = vec![];
    loop {
      match self.token.1 {
        Token::Punc(c) if c == close => {
          let end = self.token.0.end;
          self.get_token()?;
          return Ok(Cst::List(xs, (start, end).into()));
        }
        _ => xs.push(self.parse_exp()?),
      }
    }
  }
}

fn build_prog(input: &str, cst: Vec<Cst>) -> Result<Program> {
  let mut type_defs = IndexMap::new();
  let mut fun_defs = IndexMap::new();
  let mut iter = cst.into_iter().peekable();
  while let Some(Cst::List(xs, _)) = iter.peek() {
    if let Some(&Cst::Symbol(op_range)) = xs.first() {
      let op = &input[op_range.start..op_range.end];
      match op {
        "define-type" => {
          if let Some(Cst::List(xs, range)) = iter.next() {
            let (name, def) = build_type_def(input, xs, range)?;
            if type_defs.insert(name.clone(), def).is_some() {
              return Err(CompileError {
                range,
                message: format!("duplicate type {}", name),
              });
            }
          } else {
            unreachable!()
          }
        }
        "define" => {
          if let Some(Cst::List(xs, range)) = iter.next() {
            let (name, def) = build_fun_def(input, xs, range)?;
            if fun_defs.insert(name.clone(), def).is_some() {
              return Err(CompileError {
                range,
                message: format!("duplicate function {}", name),
              });
            }
          } else {
            unreachable!()
          }
        }
        _ => break,
      }
    } else {
      break;
    }
  }
  let body = if iter.peek().is_none() {
    Exp {
      kind: ExpKind::Void,
      range: (0, 0).into(),
      ty: (),
    }
  } else {
    let exp = build_exp(input, iter.next().unwrap())?;
    if iter.peek().is_none() {
      exp
    } else {
      let mut body = iter
        .map(|c| build_exp(input, c))
        .collect::<Result<Vec<_>>>()?;
      body.insert(0, exp);
      let range = (body[0].range.start, body.last().unwrap().range.end).into();
      let last = body.pop().unwrap();
      Exp {
        kind: ExpKind::Begin {
          seq: body,
          last: box last,
        },
        range,
        ty: (),
      }
    }
  };
  Ok(Program {
    type_defs,
    fun_defs,
    body,
    types: Default::default(),
  })
}

fn build_exp(input: &str, cst: Cst) -> Result<Exp> {
  match cst {
    Cst::List(xs, range) => {
      if let Some(&Cst::Symbol(sym_range)) = xs.first() {
        let op = &input[sym_range.start..sym_range.end];
        match op {
          "let" => return build_let(input, xs, range),
          "if" => return build_if(input, xs, range),
          "begin" => return build_begin(input, xs, range),
          "set!" => return build_set(input, xs, range),
          "while" => return build_while(input, xs, range),
          "print" => return build_print(input, xs, range),
          "newline" => return build_newline(input, xs, range),
          "void" => return build_void(input, xs, range),
          _ => {
            const OPERATORS: &[&str] = &[
              "read",
              "+",
              "-",
              "*",
              "quotient",
              "remainder",
              "and",
              "or",
              "not",
              "eq?",
              ">",
              "<",
              ">=",
              "<=",
              "vector",
              "vector-length",
              "vector-ref",
              "vector-set!",
              "make-vector",
              "string-append",
              "string-length",
            ];
            if let Some(&op) = OPERATORS.iter().find(|&&s| s == op) {
              let op = (sym_range, op);
              let args = xs
                .into_iter()
                .skip(1)
                .map(|c| build_exp(input, c))
                .collect::<Result<_>>()?;
              return Ok(Exp {
                kind: ExpKind::Prim { op, args },
                range,
                ty: (),
              });
            }
          }
        }
      }
      let mut xs = xs
        .into_iter()
        .map(|c| build_exp(input, c))
        .collect::<Result<Vec<_>>>()?;
      Ok(Exp {
        kind: ExpKind::Apply {
          fun: box xs.remove(0),
          args: xs,
          r#struct: None,
        },
        range,
        ty: (),
      })
    }
    Cst::Symbol(range) => Ok(Exp {
      kind: ExpKind::Var(input[range.start..range.end].to_owned()),
      range,
      ty: (),
    }),
    Cst::String(range) => {
      let mut buf = String::new();
      let mut escape = false;
      for (i, c) in input[range.start + 1..range.end - 1].chars().enumerate() {
        if escape {
          buf.push(match c {
            'n' => '\n',
            't' => '\t',
            '\\' => '\\',
            _ => {
              return Err(CompileError {
                range: (range.start + i, range.start + i + 2).into(),
                message: "unrecognized escape sequence".to_owned(),
              })
            }
          });
          escape = false;
        } else if c == '\\' {
          escape = true;
        } else {
          buf.push(c);
        }
      }
      Ok(Exp {
        kind: ExpKind::Str(buf),
        range,
        ty: (),
      })
    }
    Cst::Number(range) => Ok(Exp {
      kind: ExpKind::Int(input[range.start..range.end].parse().unwrap()),
      range,
      ty: (),
    }),
    Cst::Boolean(b, range) => Ok(Exp {
      kind: ExpKind::Bool(b),
      range,
      ty: (),
    }),
  }
}

fn make_type_def(input: &str, def: Cst) -> Result<Type> {
  match def {
    Cst::Symbol(sym_range) => match &input[sym_range.start..sym_range.end] {
      "Bool" => Ok(Type::Bool),
      "Int" => Ok(Type::Int),
      "Str" => Ok(Type::Str),
      "Void" => Ok(Type::Void),
      name => Ok(Type::Alias(TypeAlias::Pending(sym_range, name.to_owned()))),
    },
    Cst::List(mut xs, range) => {
      if let Some(Cst::Symbol(sym_range)) = xs.first() {
        let sym = &input[sym_range.start..sym_range.end];
        match sym {
          "struct" => {
            let mut fields = IndexMap::new();
            for field_def in xs.into_iter().skip(1) {
              if let Cst::List(mut xs, field_range) = field_def {
                if xs.len() == 2 {
                  let type_def = xs.pop().unwrap();
                  if let Cst::Symbol(name_range) = xs[0] {
                    let name =
                      input[name_range.start..name_range.end].to_owned();
                    let ty = make_type_def(input, type_def)?;
                    if fields.insert(name, ty).is_some() {
                      return Err(CompileError {
                        range: name_range,
                        message: "duplicate field".to_owned(),
                      });
                    }
                  } else {
                    return Err(CompileError {
                      range: field_range,
                      message: "invalid field definition".to_owned(),
                    });
                  }
                } else {
                  return Err(CompileError {
                    range: field_range,
                    message: "invalid field definition".to_owned(),
                  });
                }
              } else {
                return Err(CompileError {
                  range: field_def.range(),
                  message: "invalid field definition".to_owned(),
                });
              }
            }
            return Ok(Type::Struct(fields));
          }
          "array-of" => {
            if xs.len() == 2 {
              let ty = make_type_def(input, xs.pop().unwrap())?;
              return Ok(Type::Array(box ty));
            } else {
              return Err(CompileError {
                range,
                message: "invalid array definition".to_owned(),
              });
            }
          }
          _ => {}
        }
      }
      if xs.len() >= 2
        && matches!(xs[xs.len() - 2], Cst::Symbol(range) if
          matches!(&input[range.start..range.end], "->"))
      {
        let ret = make_type_def(input, xs.pop().unwrap())?;
        xs.pop().unwrap();
        let params = xs
          .into_iter()
          .map(|x| make_type_def(input, x))
          .collect::<Result<_>>()?;
        Ok(Type::Fun {
          params,
          ret: box ret,
        })
      } else {
        Ok(Type::Tuple(
          xs.into_iter()
            .map(|x| make_type_def(input, x))
            .collect::<Result<_>>()?,
        ))
      }
    }
    _ => Err(CompileError {
      range: def.range(),
      message: format!("invalid type def: {:?}", def),
    }),
  }
}

fn build_type_def(
  input: &str,
  mut xs: Vec<Cst>,
  range: Range,
) -> Result<(String, Type)> {
  if xs.len() != 3 {
    return Err(CompileError {
      range,
      message: "invalid define-type form".to_owned(),
    });
  }
  let def = xs.pop().unwrap();
  if let Cst::Symbol(sym_range) = xs[1] {
    let name = input[sym_range.start..sym_range.end].to_owned();
    let ty = make_type_def(input, def)?;
    Ok((name, ty))
  } else {
    Err(CompileError {
      range,
      message: "invalid define-type form".to_owned(),
    })
  }
}

fn build_fun_def(
  input: &str,
  mut xs: Vec<Cst>,
  range: Range,
) -> Result<(String, FunDef)> {
  if xs.len() < 5 {
    return Err(CompileError {
      range,
      message: "invalid function definition".to_owned(),
    });
  }
  let body = if xs.len() == 5 {
    build_exp(input, xs.pop().unwrap())?
  } else {
    build_iter_begin(input, xs.drain(4..))?
  };
  let ret = make_type_def(input, xs.pop().unwrap())?;
  if let Cst::Symbol(sym_range) = xs.pop().unwrap() {
    if &input[sym_range.start..sym_range.end] != ":" {
      return Err(CompileError {
        range,
        message: "invalid function definition".to_owned(),
      });
    }
  } else {
    return Err(CompileError {
      range,
      message: "invalid function definition".to_owned(),
    });
  }
  if let Cst::List(mut names, _) = xs.pop().unwrap() {
    if names.is_empty() {
      return Err(CompileError {
        range,
        message: "invalid function definition".to_owned(),
      });
    }
    let mut param_names = HashSet::new();
    let mut params = vec![];
    for param_def in names.drain(1..) {
      if let Cst::List(mut xs, param_range) = param_def {
        if xs.len() != 2 {
          return Err(CompileError {
            range: param_range,
            message: "invalid parameter definition".to_owned(),
          });
        }
        let ty = make_type_def(input, xs.pop().unwrap())?;
        if let Cst::Symbol(sym_range) = xs.pop().unwrap() {
          let name = &input[sym_range.start..sym_range.end];
          if !param_names.insert(name) {
            return Err(CompileError {
              range: sym_range,
              message: format!("duplicate parameter {}", name),
            });
          }
          params.push((name.to_owned(), ty));
        } else {
          return Err(CompileError {
            range: param_range,
            message: "invalid parameter definition".to_owned(),
          });
        }
      } else {
        return Err(CompileError {
          range: param_def.range(),
          message: "invalid parameter definition".to_owned(),
        });
      }
    }
    if let Cst::Symbol(sym_range) = names.pop().unwrap() {
      let name = input[sym_range.start..sym_range.end].to_owned();
      Ok((
        name,
        FunDef {
          params,
          ret,
          body,
          range,
        },
      ))
    } else {
      Err(CompileError {
        range,
        message: "invalid function definition".to_owned(),
      })
    }
  } else {
    Err(CompileError {
      range,
      message: "invalid function definition".to_owned(),
    })
  }
}

fn build_let(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() < 3 {
    return Err(CompileError {
      range,
      message: "invalid let form".to_owned(),
    });
  }

  let body = if xs.len() == 3 {
    build_exp(input, xs.pop().unwrap())?
  } else {
    let last = build_exp(input, xs.pop().unwrap())?;
    let seq: Vec<_> = xs
      .drain(2..)
      .map(|exp| build_exp(input, exp))
      .collect::<Result<_>>()?;
    let seq_range = Range {
      start: seq[0].range.start,
      end: seq.last().unwrap().range.end,
    };
    Exp {
      kind: ExpKind::Begin {
        seq,
        last: box last,
      },
      range: seq_range,
      ty: (),
    }
  };
  let inits = if let Cst::List(vars, _) = xs.pop().unwrap() {
    vars
      .into_iter()
      .map(|c| -> Result<((Range, String), Exp)> {
        if let Cst::List(mut xs, _) = c {
          if xs.len() == 2 {
            let init = build_exp(input, xs.pop().unwrap())?;
            if let Cst::Symbol(sym_range) = xs.pop().unwrap() {
              let var = input[sym_range.start..sym_range.end].to_owned();
              Ok(((sym_range, var), init))
            } else {
              Err(CompileError {
                range,
                message: "invalid let form".to_owned(),
              })
            }
          } else {
            Err(CompileError {
              range,
              message: "invalid let form".to_owned(),
            })
          }
        } else {
          Err(CompileError {
            range,
            message: "invalid let form".to_owned(),
          })
        }
      })
      .collect::<Result<Vec<_>>>()?
  } else {
    return Err(CompileError {
      range,
      message: "invalid let form".to_owned(),
    });
  };
  Ok(inits.into_iter().rev().fold(body, |body, (var, init)| Exp {
    kind: ExpKind::Let {
      var,
      init: box init,
      body: box body,
    },
    range,
    ty: (),
  }))
}

fn build_if(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() != 4 {
    return Err(CompileError {
      range,
      message: "invalid if form".to_owned(),
    });
  }
  let alt = build_exp(input, xs.pop().unwrap())?;
  let conseq = build_exp(input, xs.pop().unwrap())?;
  let cond = build_exp(input, xs.pop().unwrap())?;
  Ok(Exp {
    kind: ExpKind::If {
      cond: box cond,
      conseq: box conseq,
      alt: box alt,
    },
    range,
    ty: (),
  })
}

fn build_iter_begin<I>(input: &str, xs: I) -> Result<Exp>
where
  I: Iterator<Item = Cst>,
{
  let mut seq = xs
    .into_iter()
    .map(|exp| build_exp(input, exp))
    .collect::<Result<Vec<_>>>()?;
  let range = Range {
    start: seq[0].range.start,
    end: seq.last().unwrap().range.end,
  };
  let last = seq.pop().unwrap();
  Ok(Exp {
    kind: ExpKind::Begin {
      seq,
      last: box last,
    },
    range,
    ty: (),
  })
}

fn build_begin(input: &str, xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() > 1 {
    let mut begin = build_iter_begin(input, xs.into_iter().skip(1))?;
    begin.range = range;
    Ok(begin)
  } else {
    Err(CompileError {
      range,
      message: "invalid begin form".to_owned(),
    })
  }
}

fn build_set(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() == 3 {
    let exp = build_exp(input, xs.pop().unwrap())?;
    if let Cst::Symbol(sym_range) = xs.pop().unwrap() {
      let var = input[sym_range.start..sym_range.end].to_owned();
      Ok(Exp {
        kind: ExpKind::Set {
          var: (sym_range, var),
          exp: box exp,
        },
        range,
        ty: (),
      })
    } else {
      Err(CompileError {
        range,
        message: "invalid set! form".to_owned(),
      })
    }
  } else {
    Err(CompileError {
      range,
      message: "invalid set! form".to_owned(),
    })
  }
}

fn build_while(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  match xs.len().cmp(&3) {
    Ordering::Equal => {
      let body = build_exp(input, xs.pop().unwrap())?;
      let cond = build_exp(input, xs.pop().unwrap())?;
      Ok(Exp {
        kind: ExpKind::While {
          cond: box cond,
          body: box body,
        },
        range,
        ty: (),
      })
    }
    Ordering::Greater => {
      let body = build_iter_begin(input, xs.drain(2..))?;
      let cond = build_exp(input, xs.pop().unwrap())?;
      Ok(Exp {
        kind: ExpKind::While {
          cond: box cond,
          body: box body,
        },
        range,
        ty: (),
      })
    }
    Ordering::Less => Err(CompileError {
      range,
      message: "invalid while form".to_owned(),
    }),
  }
}

fn build_print(input: &str, xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() == 1 {
    Ok(Exp {
      kind: ExpKind::NewLine,
      range,
      ty: (),
    })
  } else {
    let args = xs
      .into_iter()
      .skip(1)
      .map(|exp| build_exp(input, exp))
      .collect::<Result<_>>()?;
    Ok(Exp {
      kind: ExpKind::Print(args),
      range,
      ty: (),
    })
  }
}

fn build_newline(_: &str, xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() == 1 {
    Ok(Exp {
      kind: ExpKind::NewLine,
      range,
      ty: (),
    })
  } else {
    Err(CompileError {
      range,
      message: "too many arguments".to_owned(),
    })
  }
}

fn build_void(_: &str, xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() == 1 {
    Ok(Exp {
      kind: ExpKind::Void,
      range,
      ty: (),
    })
  } else {
    Err(CompileError {
      range,
      message: "too many arguments".to_owned(),
    })
  }
}

pub fn parse<S: AsRef<str>>(input: S) -> Result<Program> {
  let input = input.as_ref();
  let cst = Parser::new(input).parse()?;
  build_prog(input, cst)
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn number() {
    let result = Parser::new(r#"3820"#).parse();
    let prog = Cst::Number((0, 4).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn negative_number() {
    let result = Parser::new(r#"-3820"#).parse();
    let prog = Cst::Number((0, 5).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn number_overflow() {
    let result = Parser::new(r#"123456789012345678901234567890"#).parse();
    let err = CompileError {
      range: (0, 30).into(),
      message: "integer overflow".to_owned(),
    };
    assert_eq!(result, Result::Err(err));
  }

  #[test]
  fn ascii_symbol() {
    let result = Parser::new(
      r#" 
  abc-Y7^#"#,
    )
    .parse();
    let prog = Cst::Symbol((4, 12).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn nonascii_symbol() {
    let result = Parser::new(r#"1a啊🐱   "#).parse();
    let prog = Cst::Symbol((0, 9).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn string() {
    let result = Parser::new(r#"  "abc 123"  "#).parse();
    let prog = Cst::String((2, 11).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn escape_sequence_in_string() {
    let result = Parser::new(r#"  "abc\n\t12\\3"  "#).parse();
    let prog = Cst::String((2, 16).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn comment() {
    let result = Parser::new(
      r#" ;;hff
7;;);;;;"#,
    )
    .parse();
    let prog = Cst::Number((7, 8).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn list() {
    let result = Parser::new(r#" ( 1 a  () )  "#).parse();
    let prog = Cst::List(
      vec![
        Cst::Number((3, 4).into()),
        Cst::Symbol((5, 6).into()),
        Cst::List(vec![], (8, 10).into()),
      ],
      (1, 12).into(),
    );
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn list_with_brackets() {
    let result = Parser::new(r#" [ 1 a  () ]  "#).parse();
    let prog = Cst::List(
      vec![
        Cst::Number((3, 4).into()),
        Cst::Symbol((5, 6).into()),
        Cst::List(vec![], (8, 10).into()),
      ],
      (1, 12).into(),
    );
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn comments_in_list() {
    let result = Parser::new(
      r#" [ 1 a ;;]
();aa
;;;;
];"#,
    )
    .parse();
    let prog = Cst::List(
      vec![
        Cst::Number((3, 4).into()),
        Cst::Symbol((5, 6).into()),
        Cst::List(vec![], (11, 13).into()),
      ],
      (1, 23).into(),
    );
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn multiple_exp() {
    let result = Parser::new(
      r#" aB-
1 (+) ;;;

"#,
    )
    .parse();
    let prog1 = Cst::Symbol((1, 4).into());
    let prog2 = Cst::Number((5, 6).into());
    let prog3 = Cst::List(vec![Cst::Symbol((8, 9).into())], (7, 10).into());
    assert_eq!(result, Result::Ok(vec![prog1, prog2, prog3]));
  }

  #[test]
  fn boolean() {
    let result = Parser::new(r#" #t   #f "#).parse();
    let prog1 = Cst::Boolean(true, (1, 3).into());
    let prog2 = Cst::Boolean(false, (6, 8).into());
    assert_eq!(result, Result::Ok(vec![prog1, prog2]));
  }
}
