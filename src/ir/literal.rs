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

//! GDLisp literal values.

use ordered_float::OrderedFloat;

/// A GDLisp literal value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
  Nil,
  Int(i32),
  Float(OrderedFloat<f32>),
  String(String),
  Symbol(String),
  Bool(bool),
}

impl From<i32> for Literal {
  fn from(value: i32) -> Literal {
    Literal::Int(value)
  }
}

impl From<OrderedFloat<f32>> for Literal {
  fn from(value: OrderedFloat<f32>) -> Literal {
    Literal::Float(value)
  }
}

impl From<String> for Literal {
  fn from(value: String) -> Literal {
    Literal::String(value)
  }
}

impl<'a> From<&'a str> for Literal {
  fn from(value: &'a str) -> Literal {
    Literal::String(String::from(value))
  }
}

impl From<bool> for Literal {
  fn from(value: bool) -> Literal {
    Literal::Bool(value)
  }
}
