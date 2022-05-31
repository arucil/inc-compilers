use super::explicate_control::CInfo;
use asm::{Arg, Block, ByteReg, Instr, Label, Program, Reg};
use ast::{IdxVar, Type};
use control::*;
use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use std::collections::{BTreeSet, HashMap};
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
  let types = Arena::new();
  let mut codegen = CodeGen::new(&types, use_heap);
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
    types: prog.types,
  }
}

pub struct CodeGen<'a> {
  code: Vec<Instr<IdxVar>>,
  types: &'a Arena<Type>,
  constants: IndexMap<String, String>,
  const_indices: HashMap<String, usize>,
  externs: BTreeSet<String>,
  use_heap: bool,
}

pub struct CodeGenResult {
  pub constants: IndexMap<String, String>,
  pub externs: BTreeSet<String>,
}

impl<'a> CodeGen<'a> {
  pub fn new(types: &'a Arena<Type>, use_heap: bool) -> Self {
    Self {
      code: vec![],
      types,
      constants: IndexMap::new(),
      const_indices: HashMap::new(),
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
        // ch6
        CTail::Error(CError::Length(len)) => {
          self.atom_instructions(Arg::Reg(Reg::Rdi), len);
          let label = "rt_length_error".to_owned();
          self.externs.insert(label.clone());
          self.code.push(Instr::Call {
            label,
            arity: 1,
            gc: false,
          });
          return;
        }
        CTail::Error(CError::OutOfBounds { index, len }) => {
          self.atom_instructions(Arg::Reg(Reg::Rdi), index);
          self.atom_instructions(Arg::Reg(Reg::Rsi), len);
          let label = "rt_out_of_bounds_error".to_owned();
          self.externs.insert(label.clone());
          self.code.push(Instr::Call {
            label,
            arity: 2,
            gc: false,
          });
          return;
        }
        CTail::Error(CError::DivByZero) => {
          let label = "rt_div_by_0_error".to_owned();
          self.externs.insert(label.clone());
          self.code.push(Instr::Call {
            label,
            arity: 0,
            gc: false,
          });
          return;
        }
      }
    }
  }

  fn stmt_instructions(&mut self, stmt: CStmt) {
    match stmt {
      CStmt::Assign { var, exp } => self.exp_instructions(Arg::Var(var), exp),
      // ch4
      CStmt::PrintInt(val) => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        let label = "rt_print_int".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 1,
          gc: false,
        });
      }
      CStmt::PrintBool(val) => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        let label = "rt_print_bool".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 1,
          gc: false,
        });
      }
      CStmt::PrintStr(val) => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), val);
        if self.use_heap {
          let label = "rt_print_str".to_owned();
          self.externs.insert(label.clone());
          self.code.push(Instr::Call {
            label,
            arity: 1,
            gc: false,
          });
        } else {
          let label = "rt_print_str_const".to_owned();
          self.externs.insert(label.clone());
          self.code.push(Instr::Call {
            label,
            arity: 1,
            gc: false,
          });
        }
      }
      CStmt::NewLine => {
        let label = "rt_print_newline".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 0,
          gc: false,
        })
      }
      CStmt::Read => {
        let label = "rt_read_int".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 0,
          gc: false,
        })
      }
      CStmt::TupSet {
        tup,
        fields_before,
        val,
      } => {
        let tup = atom_to_arg(tup);
        self.code.push(Instr::Mov {
          src: tup,
          dest: Arg::Reg(Reg::R11),
        });
        let offset = self.calc_field_offset(&fields_before);
        self.atom_instructions(Arg::Deref(Reg::R11, offset), val);
      }
      CStmt::ArrSet { vec, index, val } => {
        let vec = atom_to_arg(vec);
        let index = atom_to_arg(index);

        self.code.push(Instr::Mov {
          src: index,
          dest: Arg::Reg(Reg::R11),
        });
        self.code.push(Instr::Add {
          src: Arg::Imm(1),
          dest: Arg::Reg(Reg::R11),
        });
        self.code.push(Instr::Shl {
          src: Arg::Reg(Reg::R11),
          count: Arg::Imm(3),
        });
        self.code.push(Instr::Add {
          src: vec,
          dest: Arg::Reg(Reg::R11),
        });
        let val = atom_to_arg(val);
        self.code.push(Instr::Mov {
          src: val,
          dest: Arg::Deref(Reg::R11, 0),
        });
      }
      CStmt::CopyStr { dest, src, offset } => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), dest);
        self.atom_instructions(Arg::Reg(Reg::Rsi), src);
        self.atom_instructions(Arg::Reg(Reg::Rdx), offset);
        let label = "rt_copy_string".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 3,
          gc: false,
        })
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
        self.code.push(Instr::Call {
          label,
          arity: 0,
          gc: false,
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
      // ch6
      CPrim::Tuple(fields) => {
        let mut size = 0;
        let mut non_unit_fields = fields.len() as i64;
        let mut ptr_mask = 0i64;
        let mut field_offsets = vec![];
        const BASE_OFFSET: i32 = 8;
        let mut i = 0;
        for (_, ty) in &fields {
          field_offsets.push(size as i32 + BASE_OFFSET);
          match ty.resolved(self.types) {
            Type::Void => {
              non_unit_fields -= 1;
              continue;
            }
            Type::Int => size += 8,
            Type::Bool => size += 8,
            Type::Str | Type::Tuple(_) | Type::Array(_) => {
              size += 8;
              ptr_mask |= 1 << i;
            }
            Type::Func {..} => size += 8,
            Type::Struct(..) => unreachable!(),
            Type::Alias(_) => unreachable!(),
          }
          i += 1;
        }
        self.code.push(Instr::Mov {
          src: Arg::Imm(ptr_mask << 9 | non_unit_fields << 3 | 1),
          dest: Arg::Reg(Reg::Rdi),
        });
        self.code.push(Instr::Mov {
          src: Arg::Imm(size),
          dest: Arg::Reg(Reg::Rsi),
        });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::R15),
          dest: Arg::Reg(Reg::Rdx),
        });
        let label = "rt_allocate".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 3,
          gc: true,
        });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest: Arg::Reg(Reg::R11),
        });
        for ((field, ty), offset) in fields.into_iter().zip(field_offsets) {
          if ty.resolved(self.types) == Type::Void {
            continue;
          }
          self.atom_instructions(Arg::Deref(Reg::R11, offset), field);
        }
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::R11),
          dest: target,
        })
      }
      CPrim::MakeArr { len, val, ty } => {
        let len = atom_to_arg(len);

        let width = self.type_size(&ty);
        match width {
          0 => {
            self.code.push(Instr::Mov {
              src: Arg::Imm(0),
              dest: Arg::Reg(Reg::Rsi),
            });
          }
          8 => {
            self.code.push(Instr::Mov {
              src: len.clone(),
              dest: Arg::Reg(Reg::Rsi),
            });
            self.code.push(Instr::Shl {
              src: Arg::Reg(Reg::Rsi),
              count: Arg::Imm(3),
            });
          }
          _ => unreachable!(),
        }
        let ptr_mask = if ty.is_ref(self.types) { 0b1000 } else { 0 };
        let unit_mask = if width == 0 { 0b10000 } else { 0 };
        self.code.push(Instr::Mov {
          src: len,
          dest: Arg::Reg(Reg::Rdi),
        });
        self.code.push(Instr::Shl {
          src: Arg::Reg(Reg::Rdi),
          count: Arg::Imm(5),
        });
        self.code.push(Instr::Or {
          src: Arg::Imm(unit_mask | ptr_mask | 0b101),
          dest: Arg::Reg(Reg::Rdi),
        });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::R15),
          dest: Arg::Reg(Reg::Rdx),
        });
        let label = "rt_allocate".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 3,
          gc: true,
        });

        match width {
          0 => {
            self.code.push(Instr::Mov {
              src: Arg::Reg(Reg::Rax),
              dest: target,
            });
          }
          8 => {
            let val = atom_to_arg(val);
            self.code.push(Instr::Mov {
              src: Arg::Reg(Reg::Rax),
              dest: Arg::Reg(Reg::Rdi),
            });
            self.code.push(Instr::Mov {
              src: val,
              dest: Arg::Reg(Reg::Rsi),
            });
            let label = "rt_fill_array".to_owned();
            self.externs.insert(label.clone());
            self.code.push(Instr::Call {
              label,
              arity: 2,
              gc: false,
            });
            self.code.push(Instr::Mov {
              src: Arg::Reg(Reg::Rax),
              dest: target,
            });
          }
          _ => unreachable!(),
        }
      }
      CPrim::TupRef { tup, fields_before } => {
        self.atom_instructions(Arg::Reg(Reg::R11), tup);
        let offset = self.calc_field_offset(&fields_before);
        self.code.push(Instr::Mov {
          src: Arg::Deref(Reg::R11, offset),
          dest: target,
        });
      }
      CPrim::ArrRef { vec, index } => {
        let vec = atom_to_arg(vec);
        let index = atom_to_arg(index);

        self.code.push(Instr::Mov {
          src: index,
          dest: Arg::Reg(Reg::R11),
        });
        self.code.push(Instr::Add {
          src: Arg::Imm(1),
          dest: Arg::Reg(Reg::R11),
        });
        self.code.push(Instr::Shl {
          src: Arg::Reg(Reg::R11),
          count: Arg::Imm(3),
        });
        self.code.push(Instr::Add {
          src: vec,
          dest: Arg::Reg(Reg::R11),
        });
        self.code.push(Instr::Mov {
          src: Arg::Deref(Reg::R11, 0),
          dest: target,
        });
      }
      CPrim::TupLen(arg) => {
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
      CPrim::ArrLen(arg) => {
        self.atom_instructions(Arg::Reg(Reg::R11), arg);
        self.code.push(Instr::Mov {
          src: Arg::Deref(Reg::R11, 0),
          dest: target.clone(),
        });
        self.code.push(Instr::Shr {
          src: target,
          count: Arg::Imm(5),
        });
      }
      CPrim::Mul(atom1, atom2) => {
        self.atom_instructions(Arg::Reg(Reg::Rax), atom1);
        self.code.push(Instr::Xor {
          src: Arg::Reg(Reg::Rdi),
          dest: Arg::Reg(Reg::Rdi),
        });
        let arg2 = atom_to_arg(atom2);
        self.code.push(Instr::IMul(arg2));
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest: target,
        });
      }
      CPrim::Div(atom1, atom2) => {
        self.atom_instructions(Arg::Reg(Reg::Rax), atom1);
        self.code.push(Instr::Xor {
          src: Arg::Reg(Reg::Rdx),
          dest: Arg::Reg(Reg::Rdx),
        });
        let arg2 = atom_to_arg(atom2);
        self.code.push(Instr::IDiv(arg2));
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest: target,
        });
      }
      CPrim::Rem(atom1, atom2) => {
        self.atom_instructions(Arg::Reg(Reg::Rax), atom1);
        self.code.push(Instr::Xor {
          src: Arg::Reg(Reg::Rdx),
          dest: Arg::Reg(Reg::Rdx),
        });
        let arg2 = atom_to_arg(atom2);
        self.code.push(Instr::IDiv(arg2));
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rdx),
          dest: target,
        });
      }
      CPrim::AllocStr(len) => {
        self.atom_instructions(Arg::Reg(Reg::Rdi), len);
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::R15),
          dest: Arg::Reg(Reg::Rsi),
        });
        let label = "rt_alloc_string".to_owned();
        self.externs.insert(label.clone());
        self.code.push(Instr::Call {
          label,
          arity: 2,
          gc: true,
        });
        self.code.push(Instr::Mov {
          src: Arg::Reg(Reg::Rax),
          dest: target,
        });
      }
      CPrim::StrLen(arg) => {
        self.atom_instructions(Arg::Reg(Reg::R11), arg);
        self.code.push(Instr::Mov {
          src: Arg::Deref(Reg::R11, 0),
          dest: target.clone(),
        });
        self.code.push(Instr::Shr {
          src: target,
          count: Arg::Imm(3),
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
        let len = s.len();
        let label;
        if let Some(i) = self.const_indices.get(&s) {
          label = self.constants.get_index(*i).unwrap().0.clone();
        } else {
          label = format!("const_{}", self.constants.len() + 1);
          let i = self.constants.insert_full(label.clone(), s.clone()).0;
          self.const_indices.insert(s, i);
        }
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
          self.code.push(Instr::Call {
            label,
            arity: 3,
            gc: true,
          });
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

  fn calc_field_offset(&mut self, fields_before: &[Type]) -> i32 {
    let mut offset = 8;
    for field in fields_before {
      offset += self.type_size(field);
    }
    offset
  }

  fn type_size(&mut self, ty: &Type) -> i32 {
    match ty.resolved(self.types) {
      Type::Void => 0,
      Type::Int => 8,
      Type::Bool => 8,
      Type::Str => 8,
      Type::Tuple(_) => 8,
      Type::Array(_) => 8,
      Type::Func {..} => 8,
      Type::Struct(..) => unreachable!(),
      Type::Alias(_) => unreachable!(),
    }
  }
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
