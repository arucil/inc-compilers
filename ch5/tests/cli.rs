use support::TestCli;

#[test]
fn simple_while() {
  TestCli::new("ch5", "tests/fixtures").test("simple_while");
}
