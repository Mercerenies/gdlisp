
//! Helper type for printing out [`Decl`](super::decl::Decl) instances
//! with good spacing.

use crate::util::group_by::group_by;
use super::decl::{Decl, DeclF};

use std::fmt::{self, Write};

#[derive(Debug)]
pub struct SpacedDeclPrinter {
  spacing: u32,
  indentation: u32,
}

impl SpacedDeclPrinter {

  /// A default [`SpacedDeclPrinter`], suitable for top-level
  /// pretty-printing.
  pub fn new() -> Self {
    Self::default()
  }

  /// Builder method which sets the spacing between non-grouped items
  /// in the declaration list.
  pub fn with_spacing(mut self, spacing: u32) -> Self {
    self.spacing = spacing;
    self
  }

  /// Builder method which sets the base indentation level for the
  /// declarations.
  pub fn with_indentation(mut self, indentation: u32) -> Self {
    self.indentation = indentation;
    self
  }

  /// Write the sequence of declarations, in order, inserting extra
  /// newlines where it makes sense for readability.
  pub fn write_gd<'a, W: Write, I: Iterator<Item=&'a Decl>>(&self, w: &mut W, iter: I) -> Result<(), fmt::Error> {
    let mut first = true;
    for group in group_by(iter, |a, b| group_predicate(a, b)) {
      if !first {
        write_newlines(w, self.spacing)?;
      }
      first = false;
      for decl in group {
        decl.write_gd(w, self.indentation)?;
      }
    }
    Ok(())
  }

}

/// Given two adjacent declarations, determine if they can be grouped.
/// Two declarations can be grouped together if and only if both of
/// the following are true: (a) The two declarations are of the same
/// type, and (b) Both declarations are considered "short"
/// declarations.
///
/// The following declarations are considered "short": Variables,
/// constants, signals, and `pass`.
fn group_predicate(a: &Decl, b: &Decl) -> bool {
  #[allow(clippy::match_like_matches_macro)] // Looks much cleaner this way
  match (&a.value, &b.value) {
    (DeclF::VarDecl(_), DeclF::VarDecl(_)) => true,
    (DeclF::ConstDecl(_, _), DeclF::ConstDecl(_, _)) => true,
    (DeclF::SignalDecl(_, _), DeclF::SignalDecl(_, _)) => true,
    (DeclF::PassDecl, DeclF::PassDecl) => true,
    _ => false,
  }
}

/// Writes the given number of newlines to the `Write` object.
fn write_newlines(w: &mut impl Write, count: u32) -> Result<(), fmt::Error> {
  let newlines = "\n".repeat(count as usize);
  write!(w, "{}", newlines)
}

impl Default for SpacedDeclPrinter {

  fn default() -> Self {
    SpacedDeclPrinter {
      spacing: 2,
      indentation: 0,
    }
  }

}
