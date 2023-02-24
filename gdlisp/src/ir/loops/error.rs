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

//! Error types during loop primitive validation.

use crate::pipeline::source::{Sourced, SourceOffset};

use std::fmt;
use std::error::Error;

/// A `LoopPrimitiveErrorF` indicates that a looping construct, either
/// `break` or `continue`, was found outside of the scope of a loop.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoopPrimitiveErrorF {
  /// The primitive construct that was invalid.
  pub primitive: LoopPrimitive,
  /// If this is true, then the error occurred inside the lexical
  /// scope of a loop, but there was a closure in between the loop and
  /// the primitive. This is used to produce a better error message in
  /// this specific situation.
  pub is_in_closure: bool,
}

/// A [`LoopPrimitiveErrorF`] together with [`SourceOffset`] data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopPrimitiveError {
  pub value: LoopPrimitiveErrorF,
  pub pos: SourceOffset,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum LoopPrimitive {
  Break, Continue,
}

impl LoopPrimitiveError {

  pub fn new(value: LoopPrimitiveErrorF, pos: SourceOffset) -> LoopPrimitiveError {
    LoopPrimitiveError { value, pos }
  }

  pub fn break_error(pos: SourceOffset) -> LoopPrimitiveError {
    LoopPrimitiveError::new(LoopPrimitiveErrorF {
      primitive: LoopPrimitive::Break,
      is_in_closure: false,
    }, pos)
  }

  pub fn continue_error(pos: SourceOffset) -> LoopPrimitiveError {
    LoopPrimitiveError::new(LoopPrimitiveErrorF {
      primitive: LoopPrimitive::Continue,
      is_in_closure: false,
    }, pos)
  }

  pub fn in_closure(mut self) -> Self {
    self.value.is_in_closure = true;
    self
  }

}

impl Sourced for LoopPrimitiveError {
  type Item = LoopPrimitiveErrorF;

  fn get_source(&self) -> SourceOffset {
    self.pos
  }

  fn get_value(&self) -> &LoopPrimitiveErrorF {
    &self.value
  }

}

impl fmt::Display for LoopPrimitiveError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let LoopPrimitiveErrorF { primitive, is_in_closure } = &self.value;
    write!(f, "Loop primitive '{}' is not allowed outside of loops", primitive)?;
    if *is_in_closure {
    write!(f, " (Note: '{}' was found inside of a lambda or anonymous class that is nested inside of a loop; this nesting is currently not allowed)", primitive)?;
    }
    Ok(())
  }
}

impl fmt::Display for LoopPrimitive {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LoopPrimitive::Break => write!(f, "break"),
      LoopPrimitive::Continue => write!(f, "continue"),
    }
  }
}

impl Error for LoopPrimitiveError {}
