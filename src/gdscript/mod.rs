
//! Data structures for representing and manipulating valid GDScript
//! code.

pub mod expr;
pub mod stmt;
pub mod op;
pub mod literal;
pub mod pattern;
pub mod decl;
pub mod arglist;
pub mod library;
pub mod expr_wrapper;
pub mod inner_class;
pub mod metadata;

use std::fmt;
use std::convert::TryInto;

/// Indent to the given position with spaces.
///
/// # Examples
///
/// ```
/// # use gdlisp::gdscript::indent;
/// let mut str = String::new();
/// indent(&mut str, 4);
/// assert_eq!(str, "    ");
/// ```
pub fn indent<W : fmt::Write>(w : &mut W, ind: u32) -> Result<(), fmt::Error> {
  let spaces = String::from(" ").repeat(ind.try_into().unwrap());
  write!(w, "{}", spaces)
}
