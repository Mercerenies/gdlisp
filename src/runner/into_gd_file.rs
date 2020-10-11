
use crate::gdscript::decl::TopLevelClass;

use std::io::{self, Write};
use std::borrow::Borrow;

pub trait IntoGDFile {
  fn write_to_gd(&self, file: &mut impl Write) -> io::Result<()>;
}

impl IntoGDFile for TopLevelClass {
  fn write_to_gd(&self, file: &mut impl Write) -> io::Result<()> {
    write!(file, "{}", self.to_gd())
  }
}

impl<T : Borrow<str> + ?Sized> IntoGDFile for T {
  fn write_to_gd(&self, file: &mut impl Write) -> io::Result<()> {
    write!(file, "{}", self.borrow())
  }
}
