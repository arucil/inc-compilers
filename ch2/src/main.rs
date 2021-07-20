use std::env;
use std::process;
use std::fs;
use support::CompileErrorPrinter;

fn main() {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 2 {
    print_usage(&args[0]);
    process::exit(1);
  }

  let file = &args[1];
  let input = fs::read_to_string(file).unwrap_or_else(|err| {
    eprintln!("read file error");
    eprintln!("{:?}", err);
    process::exit(1);
  });

  match ch2::compile(&input) {
    Ok(output) => {
    }
    Err(err) => {
      let printer = CompileErrorPrinter::new(&input);
      printer.print(err);
      process::exit(1);
    }
  }
}

fn print_usage(prog: &str) {
  eprintln!("Usage:");
  eprintln!("    {} FILE", prog);
}
