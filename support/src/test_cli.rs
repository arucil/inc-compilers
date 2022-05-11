use assert_cmd::Command;
use scopeguard::defer;
use std::fs;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::sync::Once;

static INIT: Once = Once::new();

fn compile_runtime() {
  INIT.call_once(|| {
    run_gcc("../runtime/runtime.c");
    //run_nasm("../runtime/runtime.asm");
  });
}

pub struct TestCli {
  base_dir: PathBuf,
  bin: String,
}

impl TestCli {
  pub fn new<S: ToString, P: AsRef<Path>>(bin_name: S, base_dir: P) -> Self {
    Self {
      base_dir: base_dir.as_ref().to_owned(),
      bin: bin_name.to_string(),
    }
  }

  pub fn test<S: AsRef<str>>(&self, prog_name: S) {
    let prog = prog_name.as_ref();
    let mut cmd = Command::cargo_bin(&self.bin).unwrap();
    cmd.arg(self.base_dir.join(format!("{}.prog", prog)));
    cmd.assert().success();

    run_nasm(self.base_dir.join(format!("{}.asm", prog)));
    compile_runtime();
    run_ld(
      prog,
      &[
        PathBuf::from("../runtime/runtime.o"),
        self.base_dir.join(format!("{}.o", prog)),
      ],
    );

    defer! {
      fs::remove_file(prog).unwrap();
    }

    let inputs = self.base_dir.read_dir().unwrap().filter_map(|entry| {
      let entry = entry.unwrap();
      if let Some(name) = entry.file_name().to_str() {
        if name.starts_with(&format!("{}.", prog)) && name.ends_with(".input") {
          if name.len() == prog.len() + ".input".len() {
            return Some(name.to_owned());
          }
          if name[prog.len() + 1..name.len() - ".input".len()]
            .parse::<u32>()
            .is_ok()
          {
            return Some(name.to_owned());
          }
        }
      }
      None
    });
    let mut tested = false;
    for input in inputs {
      let output = self
        .base_dir
        .join(format!("{}.output", &input[..input.len() - ".input".len()]));
      if !output.exists() {
        panic!("missing {}", output.display());
      }
      let input = fs::read_to_string(self.base_dir.join(input)).unwrap();
      let output = fs::read_to_string(output).unwrap();
      Command::new(format!("./{}", prog))
        .write_stdin(input)
        .assert()
        .success()
        .stdout(predicates::ord::eq(output.as_bytes()));
      tested = true;
    }
    if !tested {
      panic!("{} has no input files", prog);
    }
  }
}

fn run_nasm<P: AsRef<Path>>(file: P) {
  Command::new("nasm")
    .arg("-f")
    .arg("elf64")
    .arg(file.as_ref())
    .assert()
    .success();
}

fn run_gcc<P: AsRef<Path>>(file: P) {
  let input = file.as_ref();
  let mut output = input.to_owned();
  output.set_extension("o");
  Command::new("gcc")
    .arg("-c")
    .arg(input)
    .arg("-o")
    .arg(output)
    .arg("-Wall")
    .arg("-Werror")
    .arg("-fno-builtin-exit")
    .assert()
    .success();
}

fn run_ld<P, Q>(output: P, inputs: &[Q])
where
  P: AsRef<OsStr>,
  Q: AsRef<OsStr>,
{
  Command::new("ld")
    .arg("-o")
    .arg(output)
    .args(inputs.iter())
    .assert()
    .success();
}
