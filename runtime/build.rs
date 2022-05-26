fn main() {
  println!("cargo:rerun-if-changed=runtime.c");
  cc::Build::new()
    .file("runtime.c")
    .flag("-fno-builtin")
    .opt_level(2)
    .warnings(true)
    .warnings_into_errors(true)
    .compile("runtime");
}