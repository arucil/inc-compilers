use std::fmt::{self, Write};

pub trait WritePretty {
  fn write(&self, f: &mut impl Write) -> fmt::Result;
}

impl WritePretty for () {
  fn write(&self, f: &mut impl Write) -> fmt::Result {
    Ok(())
  }
}