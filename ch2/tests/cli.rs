use support::TestCli;

#[test]
fn nested_prims() {
  TestCli::new("ch2", "tests/fixtures").test("nested_prims");
}
