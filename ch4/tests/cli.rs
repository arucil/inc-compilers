use support::TestCli;

#[test]
fn simple_if() {
  TestCli::new("ch4", "tests/fixtures").test("simple_if");
}

#[test]
fn complex_if() {
  TestCli::new("ch4", "tests/fixtures").test("complex_if");
}
