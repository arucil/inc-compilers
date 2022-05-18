use std::env;
use std::fs;
use std::path::Path;
use std::process;
use support::CompileErrorPrinter;

fn main() {
  let args = env::args().collect::<Vec<_>>();
  if args.len() != 2 {
    print_usage(&args[0]);
    process::exit(1);
  }

  let path = &args[1];
  let input = fs::read_to_string(path).unwrap_or_else(|err| {
    eprintln!("read file error");
    eprintln!("{:?}", err);
    process::exit(1);
  });

  match ch6::compile(&input, None) {
    Ok(output) => {
      let new_path = Path::new(path).with_extension("asm");
      let binary_path = new_path.with_extension("");
      fs::write(&new_path, output).unwrap_or_else(|err| {
        eprintln!("write file {} error", new_path.display());
        eprintln!("{:?}", err);
        process::exit(1);
      });
      println!("Run:");
      println!(
        "nasm -f elf64 {}.asm && \
        nasm -f elf64 runtime/runtime.asm && \
        ld -o {} runtime/runtime.o {}.o",
        binary_path.display(),
        binary_path.display(),
        binary_path.display()
      );
    }
    Err(err) => {
      let printer = CompileErrorPrinter::new(path, &input);
      printer.print(&err);
      process::exit(1);
    }
  }
}

fn print_usage(prog: &str) {
  eprintln!("Usage:");
  eprintln!("    {} FILE", prog);
}
