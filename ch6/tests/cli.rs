use support::TestCli;

#[test]
fn example_in_book() {
  TestCli::new("ch6", "tests/fixtures").test("example_in_book");
}

#[test]
fn example_in_book_struct() {
  TestCli::new("ch6", "tests/fixtures").test("example_in_book_struct");
}

#[test]
fn r#loop() {
  TestCli::new("ch6", "tests/fixtures").test("loop");
}

#[test]
fn array() {
  TestCli::new("ch6", "tests/fixtures").test("array");
}

#[test]
fn array_length_error() {
  TestCli::new("ch6", "tests/fixtures").test("array_length_error");
}

#[test]
fn array_ref_index_error() {
  TestCli::new("ch6", "tests/fixtures").test("array_ref_index_error");
}

#[test]
fn array_set_index_error() {
  TestCli::new("ch6", "tests/fixtures").test("array_set_index_error");
}

#[test]
fn gcd() {
  TestCli::new("ch6", "tests/fixtures").test("gcd");
}

#[test]
fn string() {
  TestCli::new("ch6", "tests/fixtures").test("string");
}