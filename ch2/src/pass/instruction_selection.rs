use super::explicate_control::CInfo;
use asm::{Arg, Block, ByteReg, Instr, Label, Program, Reg};
use ast::{IdxVar, Type};
use control::*;
use indexmap::{IndexMap, IndexSet};
use std::collections::BTreeSet;
use std::fmt::{self, Debug, Formatter};

pub struct Info {
  pub locals: IndexSet<IdxVar>,
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    writeln!(f, "locals: {:?}\n", self.locals)
  }
}

impl From<CInfo> for Info {
  fn from(info: CInfo) -> Self {
    Self {
      locals: info.locals,
    }
  }
}

pub fn select_instruction<OldInfo, NewInfo>(
  prog: CProgram<OldInfo>,
  use_heap: bool,
) -> Program<NewInfo, IdxVar>
where
  NewInfo: From<OldInfo>,
{
  let mut codegen = CodeGen::new(use_heap);
  let blocks = prog
    .body
    .into_iter()
    .map(|(label, tail)| (label, codegen.tail_block(tail)))
    .collect();
  let result = codegen.finish();
  Program {
    info: prog.info.into(),
    blocks,
    constants: result.constants,
    externs: result.externs,
  }
}

#[derive(Default)]
pub struct CodeGen {
  code: Vec<Instr<IdxVar>>,
  constants: IndexMap<String, String>,
  externs: BTreeSet<String>,
  use_heap: bool,
}

pub struct CodeGenResult {
  pub constants: IndexMap<String, String>,
  pub externs: BTreeSet<String>,
}

impl CodeGen {
  pub fn new(use_heap: bool) -> Self {
    Self {
      code: vec![],
      constants: IndexMap::new(),
      externs: BTreeSet::new(),
      use_heap,
    }
  }

  pub fn finish(self) -> CodeGenResult {
    CodeGenResult {
      constants: self.constants,
      externs: self.externs,
    }
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
      }
    }
  }

  fn stmt_instructions(&mut self, stmt: CStmt) {
    match stmt {
      CStmt::Assign { var, exp } => self.exp_instructions(Arg::Var(var), exp),
      // ch4
      CStmt::Print { val, ty: Type::Int } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        let label = "rt_print_int".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 0 });
      }
      CStmt::Print {
        val,
        ty: Type::Bool,
      } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        let label = "rt_print_bool".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 0 });
      }
      CStmt::Print { val, ty: Type::Str } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        let label = "rt_print_str".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 0 });
      }
      CStmt::Print { .. } => unreachable!(),
      CStmt::NewLine => {
        let label = "rt_print_newline".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 0 })
      }
      CStmt::Read => {
        let label = "rt_read_int".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 0 })
      }
      CStmt::VecSet {
        vec,
        fields_before,
        val,
      } => {
        let vec = atom_to_arg(vec);
        // TODO remove R11 requirement
        self.code.push(Instr::Mov {
          src: vec,
          dest: Arg::Reg(Reg::R11),
        });
        self.atom_instructions(
          Arg::Deref(Reg::R11, calc_field_offset(&fields_before)),
          val,
        );
      }
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
        let label = "rt_read_int".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 0 });
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
      // ch6
      CPrim::Vector(fields) => {
        let mut size = 8;
        let mut non_unit_fields = fields.len() as i64;
        let mut ptr_mask = 0i64;
        let mut field_offsets = vec![];
        for (i, (_, ty)) in fields.iter().enumerate() {
          let offset = size as i32;
          match ty {
            Type::Void => {
              non_unit_fields -= 1;
            }
            Type::Int => size += 8,
            Type::Bool => size += 8,
            Type::Str => {
              size += 8;
              ptr_mask |= 1 << i;
            }
            Type::Vector(_) => {
              size += 8;
              ptr_mask |= 1 << i;
            }
            Type::Alias(_) => todo!(),
          }
          field_offsets.push(offset);
        }
        ptr_mask >>= 1;
        self.code.push(Instr::Mov {
          src: Arg::Imm(size),
          dest: Arg::Reg(Reg::Rdi),
        });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::R15),
          dest: Arg::Reg(Reg::Rsi),
        });
        let label = "rt_allocate".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call { label, arity: 2 });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest: Arg::Reg(Reg::R11),
        });
        self.code.push(Instr::Mov {
          src: Arg::Imm(ptr_mask << 9 | non_unit_fields << 3 | 1),
          dest: Arg::Deref(Reg::R11, 0),
        });
        for ((field, ty), offset) in fields.into_iter().zip(field_offsets) {
          if ty == Type::Void {
            continue;
          }
          self.atom_instructions(Arg::Deref(Reg::R11, offset), field);
        }
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::R11),
          dest: target,
        })
      }
      CPrim::VecRef { vec, fields_before } => {
        self.atom_instructions(Arg::Reg(Reg::R11), vec);
        self.code.push(Instr::Mov {
          src: Arg::Deref(Reg::R11, calc_field_offset(&fields_before)),
          dest: target,
        });
      }
      CPrim::VecLen(arg) => {
        self.atom_instructions(Arg::Reg(Reg::R11), arg);
        self.code.push(Instr::Mov {
          src: Arg::Deref(Reg::R11, 0),
          dest: target.clone(),
        });
        self.code.push(Instr::Shr {
          src: target.clone(),
          count: Arg::Imm(3),
        });
        self.code.push(Instr::And {
          src: Arg::Imm(0b111111),
          dest: target,
        });
      }
    }
  }

  fn atom_instructions(&mut self, target: Arg<IdxVar>, atom: CAtom) {
    match atom {
      CAtom::Int(_) | CAtom::Var(_) | CAtom::Bool(_) => {
        self.code.push(Instr::Mov {
          src: atom_to_arg(atom),
          dest: target,
        });
      }
      // ch5
      CAtom::Void => {}
      CAtom::Str(s) => {
        let label = format!("const_{}", self.constants.len() + 1);
        let len = s.len();
        self.constants.insert(label.clone(), s);
        if self.use_heap {
          self.code.push(Instr::Mov {
            src: Arg::Imm(len as i64),
            dest: Arg::Reg(Reg::Rdi),
          });
          self.code.push(Instr::Mov {
            src: Arg::Label(label),
            dest: Arg::Reg(Reg::Rsi),
          });
          self.code.push(Instr::Mov {
            src: Arg::Reg(Reg::R15),
            dest: Arg::Reg(Reg::Rdx),
          });
          let label = "rt_new_string".to_owned();
          self.externs.insert(label.clone());
          self.code.push(Instr::Call { label, arity: 3 });
          self.code.push(Instr::Mov {
            src: Arg::Reg(Reg::Rax),
            dest: target,
          });
        } else {
          self.code.push(Instr::Mov {
            src: Arg::Label(label),
            dest: target,
          });
        }
      }
    }
  }
}

fn calc_field_offset(fields_before: &[Type]) -> i32 {
  let mut offset = 8;
  for field in fields_before {
    offset += match field {
      Type::Void => 0,
      Type::Int => 8,
      Type::Bool => 8,
      Type::Str => 8,
      Type::Vector(_) => 8,
      Type::Alias(_) => todo!(),
    };
  }
  offset
}

fn atom_to_arg(atom: CAtom) -> Arg<IdxVar> {
  match atom {
    CAtom::Int(n) => Arg::Imm(n),
    CAtom::Var(var) => Arg::Var(var),
    CAtom::Bool(true) => Arg::Imm(1),
    CAtom::Bool(false) => Arg::Imm(0),
    CAtom::Void => unreachable!(),
    CAtom::Str(_) => unreachable!(),
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
    let result = select_instruction::<_, Info>(prog, false);
    assert_snapshot!(result.to_string_pretty());
  }
}
