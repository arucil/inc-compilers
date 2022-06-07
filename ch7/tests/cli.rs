use support::TestCli;

#[test]
fn example_in_book() {
  TestCli::new("ch7", "tests/fixtures").test("example_in_book");
}

#[test]
fn pass_functions() {
  TestCli::new("ch7", "tests/fixtures").test("pass_functions");
}
