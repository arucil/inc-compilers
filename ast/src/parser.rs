use crate::{Program, Exp};
use support::{Range, CompileError};


pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Cst {
  List(Vec<Cst>, Range),
  Symbol(Range),
  String(Range),
  Number(Range),
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
    return self.s.chars().next()
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
      Some(c@('(' | ')' | '[' | ']')) => {
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
            None => return Err(CompileError {
              range: (start, self.i).into(),
              message: format!("unclosed string"),
            }),
            _ => {}
          }
          self.advance();
        }
        self.advance();
        self.token = ((start, self.i).into(), Token::Str);
        Ok(Token::Str)
      }
      Some(c) if !"])".contains(c) => {
        self.advance();
        let mut c = self.peek();
        while matches!(c, Some(c) if !c.is_whitespace() && !"()[];'\"".contains(c)) {
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
      })
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
        message: format!("unexpected EOF"),
      }),
      Token::Punc(c) => Err(CompileError {
        range: self.token.0,
        message: format!("unexpected char '{}'", c),
      })
    }
  }

  fn parse_sym(&mut self, range: Range) -> Result<Cst> {
    self.get_token()?;
    if let Err(err) = self.input[range.start..range.end].parse::<i64>() {
      use std::num::IntErrorKind;
      if let IntErrorKind::PosOverflow | IntErrorKind::NegOverflow = err.kind() {
        return Err(CompileError {
          range,
          message: format!("integer overflow"),
        });
      } else {
        return Ok(Cst::Symbol(range));
      }
    } else {
      return Ok(Cst::Number(range));
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
        _ => xs.push(self.parse_exp()?)
      }
    }
  }
}

fn build_prog(input: &str, cst: Vec<Cst>) -> Result<Program> {
  Ok(Program {
    body: cst.into_iter().map(|c| build_exp(input, c)).collect::<Result<_>>()?,
  })
}

fn build_exp(input: &str, cst: Cst) -> Result<(Range, Exp)> {
  fn make_prim(
    input: &str,
    range: Range,
    sym_range: Range,
    op: &str,
    xs: Vec<Cst>,
  ) -> Result<(Range, Exp)> {
    let op = (sym_range, op.to_owned());
    let args = xs.into_iter()
      .skip(1)
      .map(|c| build_exp(input, c))
      .collect::<Result<_>>()?;
    Ok((range, Exp::Prim { op, args }))
  }

  match cst {
    Cst::List(xs, range) => {
      if let Some(&Cst::Symbol(sym_range)) = xs.get(0) {
        let op = &input[sym_range.start..sym_range.end];
        match op {
          "read" => {
            if xs.len() == 1 {
              make_prim(input, range, sym_range, op, xs)
            } else {
              Err(CompileError {
                range,
                message: format!("read does not take arguments"),
              })
            }
          }
          "-" => {
            if xs.len() == 2 {
              make_prim(input, range, sym_range, op, xs)
            } else {
              Err(CompileError {
                range,
                message: format!("- expects one argument"),
              })
            }
          }
          "+" => {
            if xs.len() == 3 {
              make_prim(input, range, sym_range, op, xs)
            } else {
              Err(CompileError {
                range,
                message: format!("+ expects two arguments"),
              })
            }
          }
          "let" => build_let(input, xs, range),
          _ => Err(CompileError {
            range,
            message: format!("unrecognized form"),
          })
        }
      } else {
        return Err(CompileError {
          range,
          message: format!("unrecognized form"),
        })
      }
    }
    Cst::Symbol(range) => {
      Ok((range, Exp::Var(input[range.start..range.end].to_owned())))
    }
    Cst::String(range) => {
      let mut buf = String::new();
      let mut escape = false;
      for (i, c) in input[range.start + 1..range.end - 1].chars().enumerate() {
        if escape {
          buf.push(match c {
            'n' => '\n',
            't' => '\t',
            '\\' => '\\',
            _ => return Err(CompileError {
              range: (range.start + i, range.start + i + 2).into(),
              message: format!("unrecognized escape sequence"),
            })
          });
          escape = false;
        } else if c == '\\' {
          escape = true;
        } else {
          buf.push(c);
        }
      }
      Ok((range, Exp::Str(buf)))
    }
    Cst::Number(range) => {
      Ok((range, Exp::Int(input[range.start..range.end].parse().unwrap())))
    }
  }
}

fn build_let(
  input: &str,
  mut xs: Vec<Cst>,
  range: Range,
) -> Result<(Range, Exp)> {
  if xs.len() == 3 {
    let body = build_exp(input, xs.pop().unwrap())?;
    let inits = if let Cst::List(vars, _) = xs.pop().unwrap() {
      vars.into_iter().map(|c| -> Result<((Range, String), (Range, Exp))> {
        if let Cst::List(mut xs, _) = c {
          if xs.len() == 2 {
            let init = build_exp(input, xs.pop().unwrap())?;
            if let Cst::Symbol(sym_range) = xs.pop().unwrap() {
              let var = input[sym_range.start..sym_range.end].to_owned();
              Ok(((sym_range, var), init))
            } else {
              Err(CompileError {
                range,
                message: format!("invalid let form"),
              })
            }
          } else {
            Err(CompileError {
              range,
              message: format!("invalid let form"),
            })
          }
        } else {
          Err(CompileError {
            range,
            message: format!("invalid let form"),
          })
        }
      }).collect::<Result<Vec<_>>>()?
    } else {
      return Err(CompileError {
        range,
        message: format!("invalid let form"),
      })
    };
    Ok(inits.into_iter().rev()
      .fold(body, |body, (var, init)|
        (range, Exp::Let { var, init: box init, body: box body })))
  } else {
    Err(CompileError {
      range,
      message: format!("invalid let form"),
    })
  }
}

pub fn parse<S: AsRef<str>>(
  input: S,
) -> Result<Program> {
  let input = input.as_ref();
  let cst = Parser::new(input).parse()?;
  build_prog(input, cst)
}

#[cfg(test)]
mod tests {
  use pretty_assertions::assert_eq;
  use super::*;

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
      message: format!("integer overflow"),
    };
    assert_eq!(result, Result::Err(err));
  }

  #[test]
  fn ascii_symbol() {
    let result = Parser::new(r#" 
  abc-Y7^#"#).parse();
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
    let result = Parser::new(r#" ;;hff
7;;);;;;"#).parse();
    let prog = Cst::Number((7, 8).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn list() {
    let result = Parser::new(r#" ( 1 a  () )  "#).parse();
    let prog = Cst::List(vec![
      Cst::Number((3, 4).into()),
      Cst::Symbol((5, 6).into()),
      Cst::List(vec![], (8, 10).into()),
    ], (1, 12).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn list_with_brackets() {
    let result = Parser::new(r#" [ 1 a  () ]  "#).parse();
    let prog = Cst::List(vec![
      Cst::Number((3, 4).into()),
      Cst::Symbol((5, 6).into()),
      Cst::List(vec![], (8, 10).into()),
    ], (1, 12).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn comments_in_list() {
    let result = Parser::new(r#" [ 1 a ;;]
();aa
;;;;
];"#).parse();
    let prog = Cst::List(vec![
      Cst::Number((3, 4).into()),
      Cst::Symbol((5, 6).into()),
      Cst::List(vec![], (11, 13).into()),
    ], (1, 23).into());
    assert_eq!(result, Result::Ok(vec![prog]));
  }

  #[test]
  fn multiple_exp() {
    let result = Parser::new(r#" aB-
1 (+) ;;;

"#).parse();
    let prog1 = Cst::Symbol((1, 4).into());
    let prog2 = Cst::Number((5, 6).into());
    let prog3 = Cst::List(vec![Cst::Symbol((8, 9).into())], (7, 10).into());
    assert_eq!(result, Result::Ok(vec![prog1, prog2, prog3]));
  }
}
