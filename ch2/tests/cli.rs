use std::fs;
use assert_cmd::Command;
use scopeguard::defer;

#[test]
fn nested_prims() {
  let mut cmd = Command::cargo_bin("ch2").unwrap();
  cmd.arg("tests/fixtures/nested_prims.prog");
  cmd.assert().success();

  run_nasm("tests/fixtures/nested_prims.asm");
  run_nasm("../runtime/runtime.asm");
  run_ld("nested_prims", &["../runtime/runtime.o", "tests/fixtures/nested_prims.o"]);

  defer!{
    fs::remove_file("nested_prims").unwrap();
  }

  let input = fs::read_to_string("tests/fixtures/nested_prims.input").unwrap();
  let output = fs::read_to_string("tests/fixtures/nested_prims.output").unwrap();
  Command::new("nested_prims")
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