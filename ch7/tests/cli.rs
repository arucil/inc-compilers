use support::TestCli;

#[test]
fn example_in_book() {
  TestCli::new("ch7", "tests/fixtures").test("example_in_book");
}

#[test]
fn pass_functions() {
  TestCli::new("ch7", "tests/fixtures").test("pass_functions");
}


#[test]
fn fib() {
  TestCli::new("ch7", "tests/fixtures").test("fib");
}

#[test]
fn even_odd() {
  TestCli::new("ch7", "tests/fixtures").test("even_odd");
}

#[test]
fn vector_prod() {
  TestCli::new("ch7", "tests/fixtures").test("vector-prod");
}