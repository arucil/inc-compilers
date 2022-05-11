use super::explicate_control::CInfo;
use asm::{Arg, Block, ByteReg, Instr, Label, Program, Reg};
use ast::{IdxVar, Type};
use control::*;
use indexmap::{IndexMap, IndexSet};
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}\n", self.locals)
  }
}

pub fn select_instruction(prog: CProgram<CInfo>) -> Program<Info, IdxVar> {
  let mut code_gen = CodeGen::new();
  Program {
    info: Info {
      locals: prog.info.locals,
    },
    blocks: prog
      .body
      .into_iter()
      .map(|(label, tail)| (label, code_gen.tail_block(tail)))
      .collect(),
    constants: code_gen.into_constants(),
  }
}

#[derive(Default)]
pub struct CodeGen {
  code: Vec<Instr<IdxVar>>,
  constants: IndexMap<String, String>,
}

impl CodeGen {
  pub fn new() -> Self {
    Self {
      code: vec![],
      constants: IndexMap::new(),
    }
  }

  pub fn into_constants(self) -> IndexMap<String, String> {
    self.constants
  }

  pub fn tail_block(&mut self, tail: CTail) -> Block<IdxVar> {
    self.code.clear();
    self.tail_instructions(tail);
    Block {
      global: false,
      code: std::mem::take(&mut self.code),
    }
  }

  fn tail_instructions(&mut self, mut tail: CTail) {
    loop {
      match tail {
        CTail::Return(exp) => {
          self.exp_instructions(Arg::Reg(Reg::Rax), exp);
          self.code.push(Instr::Jmp(Label::Conclusion));
          return;
        }
        CTail::Seq(stmt, new_tail) => {
          self.stmt_instructions(stmt);
          tail = *new_tail;
        }
        // ch4
        CTail::Goto(label) => {
          self.code.push(Instr::Jmp(label));
          return;
        }
        CTail::If {
          cmp,
          lhs,
          rhs,
          conseq,
          alt,
        } => {
          let lhs = atom_to_arg(lhs);
          let rhs = atom_to_arg(rhs);
          self.code.push(Instr::Cmp {
            dest: lhs,
            src: rhs,
          });
          self.code.push(Instr::JumpIf {
            cmp: cmp.into(),
            label: conseq,
          });
          self.code.push(Instr::Jmp(alt));
          return;
        }
        _ => unimplemented!(),
      }
    }
  }

  fn stmt_instructions(&mut self, stmt: CStmt) {
    match stmt {
      CStmt::Assign { var, exp } => self.exp_instructions(Arg::Var(var), exp),
      // ch4
      CStmt::Print { val, ty: Type::Int } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        self.code.push(Instr::Call {
          label: "print_int".to_owned(),
          arity: 0,
        });
      }
      CStmt::Print {
        val,
        ty: Type::Bool,
      } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        self.code.push(Instr::Call {
          label: "print_bool".to_owned(),
          arity: 0,
        });
      }
      CStmt::Print { val, ty: Type::Str } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        self.code.push(Instr::Call {
          label: "print_str".to_owned(),
          arity: 0,
        });
      }
      CStmt::NewLine => self.code.push(Instr::Call {
        label: "print_newline".to_owned(),
        arity: 0,
      }),
      CStmt::Read => self.code.push(Instr::Call {
        label: "read_int".to_owned(),
        arity: 0,
      }),
      _ => unimplemented!("{:?}", stmt),
    }
  }

  fn exp_instructions(&mut self, target: Arg<IdxVar>, exp: CExp) {
    match exp {
      CExp::Atom(atom) => self.atom_instructions(target, atom),
      CExp::Prim(prim) => self.prim_instructions(target, prim),
    }
  }

  fn prim_instructions(&mut self, target: Arg<IdxVar>, prim: CPrim) {
    match prim {
      CPrim::Read => {
        self.code.push(Instr::Call {
          label: "read_int".to_owned(),
          arity: 0,
        });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest: target,
        });
      }
      CPrim::Neg(atom) => {
        self.atom_instructions(target.clone(), atom);
        self.code.push(Instr::Neg(target));
      }
      CPrim::Add(atom1, atom2) => {
        self.atom_instructions(target.clone(), atom1);
        let arg = atom_to_arg(atom2);
        self.code.push(Instr::Add {
          src: arg,
          dest: target,
        });
      }
      CPrim::Sub(atom1, atom2) => {
        self.atom_instructions(target.clone(), atom1);
        let arg = atom_to_arg(atom2);
        self.code.push(Instr::Sub {
          src: arg,
          dest: target,
        });
      }
      // ch4
      CPrim::Not(atom) => {
        self.atom_instructions(target.clone(), atom);
        self.code.push(Instr::Xor {
          src: Arg::Imm(1),
          dest: target,
        });
      }
      CPrim::Cmp(cmp, atom1, atom2) => {
        let arg1 = atom_to_arg(atom1);
        let arg2 = atom_to_arg(atom2);
        self.code.push(Instr::Cmp {
          dest: arg1,
          src: arg2,
        });
        self.code.push(Instr::SetIf {
          cmp: cmp.into(),
          dest: Arg::ByteReg(ByteReg::Al),
        });
        self.code.push(Instr::Movzx {
          src: Arg::ByteReg(ByteReg::Al),
          dest: target,
        });
      }
      _ => unimplemented!(),
    }
  }

  fn atom_instructions(&mut self, target: Arg<IdxVar>, atom: CAtom) {
    match atom {
      CAtom::Int(n) => {
        self.code.push(Instr::Mov {
          src: Arg::Imm(n),
          dest: target,
        });
      }
      CAtom::Var(var) => {
        self.code.push(Instr::Mov {
          src: Arg::Var(var),
          dest: target,
        });
      }
      // ch4
      CAtom::Bool(true) => {
        self.code.push(Instr::Mov {
          src: Arg::Imm(1),
          dest: target,
        });
      }
      CAtom::Bool(false) => {
        self.code.push(Instr::Mov {
          src: Arg::Imm(0),
          dest: target,
        });
      }
      // ch5
      CAtom::Void => {}
      CAtom::Str(s) => {
        let label = format!("const_{}", self.constants.len() + 1);
        self.constants.insert(label.clone(), s);
        self.code.push(Instr::Mov {
          src: Arg::Label(label),
          dest: target,
        });
      }
    }
  }
}

fn atom_to_arg(atom: CAtom) -> Arg<IdxVar> {
  match atom {
    CAtom::Int(n) => Arg::Imm(n),
    CAtom::Var(var) => Arg::Var(var),
    CAtom::Bool(true) => Arg::Imm(1),
    CAtom::Bool(false) => Arg::Imm(0),
    CAtom::Void => unimplemented!(),
    CAtom::Str(_) => todo!("compare strings"),
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use ast::*;
  use insta::assert_snapshot;

  #[test]
  fn nested_prims() {
    let prog =
      parse(r#"(let ([x (read)] [y (+ 2 3)]) (+ (- (read)) (+ y (- 2))))"#)
        .unwrap();
    let prog = uniquify::uniquify(prog).unwrap();
    let prog = remove_complex_operands::remove_complex_operands(prog);
    let prog = explicate_control::explicate_control(prog);
    let result = select_instruction(prog);
    assert_snapshot!(result.to_string_pretty());
  }
}
