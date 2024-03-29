// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! GDScript literal values.
//!
//! This module represents the subset of [GDScript
//! expressions](super::expr) which are literal values, i.e. which
//! evaluate to themselves. This includes literal integers, strings,
//! and the special value `null`.

use super::is_valid_node_path;
use crate::ir::literal::{Literal as IRLiteral};
use crate::sxp::string::insert_escapes;

use ordered_float::OrderedFloat;
use serde::{Serialize, Deserialize};

use std::convert::TryFrom;
use std::ops::Deref;
use std::fmt;

/// The type of GDScript literal values.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Literal {
  Int(i32),
  Float(LiteralFloat),
  String(String),
  NodeLiteral(String),
  NodePathLiteral(String),
  Null,
  Bool(bool),
}

/// A literal floating-point value, with reflexive equality semantics.
/// The only difference between this type and [`OrderedFloat`] is that
/// the former supports serde.
#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct LiteralFloat(pub f32);

/// In most cases, we can convert an
/// [`ir::literal::Literal`](IRLiteral) to a [`Literal`]. However,
/// there are a handful of corner cases where `ir::literal::Literal`
/// compiles to something nontrivial in GDScript. In those cases, this
/// error will be returned.
#[derive(Debug, Clone, Default)]
pub struct IRToExprLiteralError;

impl Literal {

  /// Convert a GDScript literal to a string. The result will contain
  /// valid GDScript syntax.
  pub fn to_gd(&self) -> String {
    match self {
      Literal::Int(n) => n.to_string(),
      Literal::String(s) => format!("\"{}\"", insert_escapes(s)),
      Literal::NodeLiteral(s) => {
        if is_valid_node_path(s) {
          format!("${}", s)
        } else {
          format!("$\"{}\"", insert_escapes(s))
        }
      }
      Literal::NodePathLiteral(s) => format!("@\"{}\"", insert_escapes(s)),
      Literal::Null => String::from("null"),
      Literal::Bool(b) => if *b { String::from("true") } else { String::from("false") },
      Literal::Float(f) => format!("{:e}", **f),
    }
  }

}

impl From<i32> for Literal {
  fn from(x: i32) -> Literal {
    Literal::Int(x)
  }
}

impl From<OrderedFloat<f32>> for Literal {
  fn from(x: OrderedFloat<f32>) -> Literal {
    Literal::Float(LiteralFloat(*x))
  }
}

impl From<String> for Literal {
  fn from(x: String) -> Literal {
    Literal::String(x)
  }
}

impl<'a> From<&'a str> for Literal {
  fn from(x: &'a str) -> Literal {
    Literal::String(String::from(x))
  }
}

impl From<bool> for Literal {
  fn from(x: bool) -> Literal {
    Literal::Bool(x)
  }
}

impl TryFrom<IRLiteral> for Literal {
  type Error = IRToExprLiteralError;

  fn try_from(value: IRLiteral) -> Result<Literal, Self::Error> {
    match value {
      IRLiteral::Nil => Ok(Literal::Null),
      IRLiteral::Int(n) => Ok(Literal::Int(n)),
      IRLiteral::Float(f) => Ok(Literal::Float(LiteralFloat(*f))),
      IRLiteral::String(s) => Ok(Literal::String(s)),
      IRLiteral::Symbol(_) => Err(IRToExprLiteralError), // Doesn't compile to a GDScript literal
      IRLiteral::Bool(b) => Ok(Literal::Bool(b)),
    }
  }

}

impl PartialEq for LiteralFloat {

  /// Compares the two floats for equality using [`OrderedFloat`]
  /// semantics, so NaN compares equal to itself.
  fn eq(&self, other: &Self) -> bool {
    OrderedFloat(**self) == OrderedFloat(**other)
  }

}

impl Eq for LiteralFloat {}

impl Deref for LiteralFloat {
  type Target = f32;

  fn deref(&self) -> &f32 {
    &self.0
  }

}

impl fmt::Debug for LiteralFloat {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn literal_test() {
    assert_eq!(Literal::Int(10).to_gd(), "10");
    assert_eq!(Literal::Null.to_gd(), "null");
    assert_eq!(Literal::Bool(false).to_gd(), "false");
    assert_eq!(Literal::Bool(true).to_gd(), "true");
  }

  #[test]
  fn string_test() {
    assert_eq!(Literal::String("foo".to_owned()).to_gd(), "\"foo\"");
    assert_eq!(Literal::String("foo\"bar".to_owned()).to_gd(), "\"foo\\\"bar\"");
  }

  #[test]
  fn node_path_test() {
    assert_eq!(Literal::NodeLiteral("foo".to_owned()).to_gd(), "$foo");
    assert_eq!(Literal::NodeLiteral("foo\"bar".to_owned()).to_gd(), "$\"foo\\\"bar\"");
    assert_eq!(Literal::NodeLiteral("foo bar".to_owned()).to_gd(), "$\"foo bar\"");
  }

  #[test]
  fn node_path_literal_test() {
    assert_eq!(Literal::NodePathLiteral("foo".to_owned()).to_gd(), "@\"foo\"");
    assert_eq!(Literal::NodePathLiteral("foo\"bar".to_owned()).to_gd(), "@\"foo\\\"bar\"");
    assert_eq!(Literal::NodePathLiteral("foo bar".to_owned()).to_gd(), "@\"foo bar\"");
  }

}
