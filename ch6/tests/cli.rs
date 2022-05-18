use support::TestCli;

#[test]
fn example_in_book() {
  TestCli::new("ch6", "tests/fixtures").test("example_in_book");
}