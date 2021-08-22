use assert_cmd::Command;
use scopeguard::defer;
use std::ffi;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Once;

static INIT: Once = Once::new();

fn compile_runtime() {
  INIT.call_once(|| {
    run_nasm("../runtime/runtime.asm");
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

    let input =
      fs::read_to_string(self.base_dir.join(format!("{}.input", prog)))
        .unwrap();
    let output =
      fs::read_to_string(self.base_dir.join(format!("{}.output", prog)))
        .unwrap();
    Command::new(prog)
      .write_stdin(input)
      .assert()
      .success()
      .stdout(predicates::ord::eq(output.as_bytes()));
  }
}

fn run_nasm<P: AsRef<ffi::OsStr>>(file: P) {
  Command::new("nasm")
    .arg("-f")
    .arg("elf64")
    .arg(file)
    .assert()
    .success();
}

fn run_ld<P, Q>(output: P, inputs: &[Q])
where
  P: AsRef<ffi::OsStr>,
  Q: AsRef<ffi::OsStr>,
{
  Command::new("ld")
    .arg("-o")
    .arg(output)
    .args(inputs.iter())
    .assert()
    .success();
}
