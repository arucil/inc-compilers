use std::cmp::Ordering;

use crate::{Exp, ExpKind, Program, StructDef, TypeDef};
use indexmap::IndexMap;
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
  let mut defs = IndexMap::new();
  let mut iter = cst.into_iter().peekable();
  while let Some(Cst::List(xs, _)) = iter.peek() {
    if let Some(&Cst::Symbol(op_range)) = xs.first() {
      let op = &input[op_range.start..op_range.end];
      if op == "define-struct" {
        if let Some(Cst::List(xs, range)) = iter.next() {
          let (name, st) = build_struct_def(input, xs, range)?;
          defs.insert(name, st);
        } else {
          unreachable!()
        }
      } else {
        break;
      }
    } else {
      break;
    }
  }
  Ok(Program {
    defs,
    body: iter.map(|c| build_exp(input, c)).collect::<Result<_>>()?,
    types: Default::default(),
  })
}

fn build_exp(input: &str, cst: Cst) -> Result<Exp> {
  match cst {
    Cst::List(xs, range) => {
      if let Some(&Cst::Symbol(sym_range)) = xs.first() {
        let op = &input[sym_range.start..sym_range.end];
        match op {
          "let" => build_let(input, xs, range),
          "if" => build_if(input, xs, range),
          "begin" => build_begin(input, xs, range),
          "set!" => build_set(input, xs, range),
          "while" => build_while(input, xs, range),
          "print" => build_print(input, xs, range),
          "void" => build_void(input, xs, range),
          _ => {
            const OPERATORS: &[&str] = &[
              "read",
              "+",
              "-",
              "*",
              "/",
              "%",
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
              Ok(Exp {
                kind: ExpKind::Prim { op, args },
                range,
                ty: (),
              })
            } else {
              let name = (sym_range, op.to_owned());
              let args = xs
                .into_iter()
                .skip(1)
                .map(|c| build_exp(input, c))
                .collect::<Result<_>>()?;
              Ok(Exp {
                kind: ExpKind::Call { name, args },
                range,
                ty: (),
              })
            }
          }
        }
      } else {
        Err(CompileError {
          range,
          message: "unrecognized form".to_owned(),
        })
      }
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

fn build_struct_def(
  input: &str,
  xs: Vec<Cst>,
  range: Range,
) -> Result<(String, StructDef)> {
  if let Some(Cst::Symbol(sym_range)) = xs.get(1) {
    let name = input[sym_range.start..sym_range.end].to_owned();
    let mut fields = IndexMap::new();
    for field_def in xs.into_iter().skip(2) {
      if let Cst::List(xs, field_range) = field_def {
        if let [Cst::Symbol(name_range), Cst::Symbol(type_range)] =
          xs.as_slice()
        {
          let name = input[name_range.start..name_range.end].to_owned();
          let ty = match &input[type_range.start..type_range.end] {
            "Bool" => TypeDef::Bool,
            "Int" => TypeDef::Int,
            "Str" => TypeDef::Str,
            "Void" => TypeDef::Void,
            name => TypeDef::Alias(*type_range, name.to_owned()),
          };
          if fields.insert(name, ty).is_some() {
            return Err(CompileError {
              range: *name_range,
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
          range: field_def.range(),
          message: "invalid field definition".to_owned(),
        });
      }
    }
    Ok((name, StructDef(fields)))
  } else {
    Err(CompileError {
      range,
      message: "invalid define-struct form".to_owned(),
    })
  }
}

fn build_let(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() >= 3 {
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
  } else {
    Err(CompileError {
      range,
      message: "invalid let form".to_owned(),
    })
  }
}

fn build_if(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() == 4 {
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
  } else {
    Err(CompileError {
      range,
      message: "invalid if form".to_owned(),
    })
  }
}

fn build_begin(input: &str, mut xs: Vec<Cst>, range: Range) -> Result<Exp> {
  if xs.len() > 1 {
    let last = build_exp(input, xs.pop().unwrap())?;
    let seq = xs
      .into_iter()
      .skip(1)
      .map(|exp| build_exp(input, exp))
      .collect::<Result<_>>()?;
    Ok(Exp {
      kind: ExpKind::Begin {
        seq,
        last: box last,
      },
      range,
      ty: (),
    })
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
      let last = build_exp(input, xs.pop().unwrap())?;
      let seq: Vec<_> = xs
        .drain(2..)
        .map(|exp| build_exp(input, exp))
        .collect::<Result<_>>()?;
      let seq_range = Range {
        start: seq[0].range.start,
        end: seq.last().unwrap().range.end,
      };
      let cond = build_exp(input, xs.pop().unwrap())?;
      Ok(Exp {
        kind: ExpKind::While {
          cond: box cond,
          body: box Exp {
            kind: ExpKind::Begin {
              seq,
              last: box last,
            },
            range: seq_range,
            ty: (),
          },
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
