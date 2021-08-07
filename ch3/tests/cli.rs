use support::TestCli;

#[test]
fn nested_prims() {
  TestCli::new("ch3", "tests/fixtures").test("nested_prims");
}

#[test]
fn spilled() {
  TestCli::new("ch3", "tests/fixtures").test("spilled");
}