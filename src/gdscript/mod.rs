
pub mod expr;
pub mod stmt;
pub mod op;
pub mod literal;
pub mod pattern;
pub mod decl;
pub mod arglist;
pub mod library;
pub mod expr_wrapper;

use std::fmt;
use std::convert::TryInto;

pub fn indent<W : fmt::Write>(w : &mut W, ind: u32) -> Result<(), fmt::Error> {
  let spaces = String::from(" ").repeat(ind.try_into().unwrap());
  write!(w, "{}", spaces)
}
