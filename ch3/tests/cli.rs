use assert_cmd::Command;
use scopeguard::defer;
use std::fs;

#[test]
fn nested_prims() {
  test_program("nested_prims");
}

#[test]
fn spilled() {
  test_program("spilled");
}

fn test_program(prog: &str) {
  let mut cmd = Command::cargo_bin("ch3").unwrap();
  cmd.arg(format!("tests/fixtures/{}.prog", prog));
  cmd.assert().success();

  run_nasm(&format!("tests/fixtures/{}.asm", prog));
  run_nasm("../runtime/runtime.asm");
  run_ld(
    prog,
    &[
      "../runtime/runtime.o",
      &format!("tests/fixtures/{}.o", prog),
    ],
  );

  defer! {
    fs::remove_file(prog).unwrap();
  }

  let input =
    fs::read_to_string(format!("tests/fixtures/{}.input", prog)).unwrap();
  let output =
    fs::read_to_string(format!("tests/fixtures/{}.output", prog)).unwrap();
  Command::new(prog)
    .write_stdin(input)
    .assert()
    .success()
    .stdout(predicates::str::diff(output));
}

fn run_nasm(file: &str) {
  Command::new("nasm")
    .arg("-f")
    .arg("elf64")
    .arg(file)
    .assert()
    .success();
}

fn run_ld(output: &str, inputs: &[&str]) {
  Command::new("ld")
    .arg("-o")
    .arg(output)
    .args(inputs.iter())
    .assert()
    .success();
}
