
//! This module defines [`IntoGDFile`], a trait for objects which can
//! be written to disk as GDScript source files.
//!
//! The primary implementor is [`TopLevelClass`], which represents
//! GDScript source code as a Rust-side AST.

use crate::gdscript::decl::TopLevelClass;

use std::io::{self, Write};
use std::borrow::Borrow;

/// A trait for objects which can be reasonably written to a file as
/// GDScript source code.
pub trait IntoGDFile {
  /// Writes the contents of `self` to `file` as GDScript source code.
  fn write_to_gd(&self, file: &mut impl Write) -> io::Result<()>;
}

/// A [`TopLevelClass`] can be written to a file using
/// [`to_gd`](TopLevelClass::to_gd()).
impl IntoGDFile for TopLevelClass {
  fn write_to_gd(&self, file: &mut impl Write) -> io::Result<()> {
    write!(file, "{}", self.to_gd())
  }
}

/// A string of GDScript source code can be written to a file
/// verbatim.
impl<T : Borrow<str> + ?Sized> IntoGDFile for T {
  fn write_to_gd(&self, file: &mut impl Write) -> io::Result<()> {
    write!(file, "{}", self.borrow())
  }
}
