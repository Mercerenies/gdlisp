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

//! All of the operators recognized by GDScript.

use serde::{Serialize, Deserialize};

/// Unary operators.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub enum UnaryOp {
  BitNot, Negate, Not,
}

/// Binary operators.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub enum BinaryOp {
  Times, Div, Mod, Add, Sub, LShift, RShift, BitAnd, BitXor,
  BitOr, LT, GT, Eq, NE, LE, GE, Is, In, And, Or, Cast,
}

/// Assignment operators.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub enum AssignOp {
  Eq, Add, Sub, Times, Div, Mod, BitAnd, BitOr,
}

/// Ternary operators.
///
/// There is only one ternary op in Godot: if conditionals. Hence,
/// this struct is a singleton whose unique value represents ternary
/// if conditions.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct TernaryOp;

/// Information about an operator.
#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct OperatorInfo {
  /// The operator's precedence. Operators with higher precedence will
  /// bind more tightly.
  pub precedence: i32,
  /// The operator's name, as a string.
  pub name: &'static str,
  /// How spaces should be applied around an operator.
  pub padding: Padding,
}

/// This enum describes different mechanisms for providing padding
/// around an operator.
///
/// There are some operators in GDScript that require spaces around
/// them, such as `is`. There are many more that, while surrounding
/// spaces are not required, tend to be more readable if padding is
/// added. Finally, there are some (especially unary operators) which
/// simply do not require spaces for readability.
///
/// An [operator's info](OperatorInfo) contains a [Padding] which
/// describes how that operator would like to be spaced.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum Padding {
  /// The operator is ambivalent to padding. It may be added if needed
  /// for some other reason but is not required.
  NotRequired,
  /// The operator is more readable if spaces are added. It is not
  /// incorrect to omit spaces, but they should be added unless there
  /// is a good reason not to.
  Preferred,
  /// The operator requires spaces around it. Failing to include them
  /// may result in incorrect code.
  Required,
}

fn info(prec: i32, name: &'static str, padding: Padding) -> OperatorInfo {
  OperatorInfo { precedence: prec, name: name, padding: padding }
}

/// All of the operator types in this module can produce an
/// [OperatorInfo] value describing their properties. This trait
/// describes any type which can produce such an [OperatorInfo].
pub trait OperatorHasInfo {
  /// Produce [OperatorInfo].
  fn op_info(&self) -> OperatorInfo;
}

impl OperatorHasInfo for UnaryOp {
  fn op_info(&self) -> OperatorInfo {
    match self {
      UnaryOp::BitNot => info(17, "~", Padding::NotRequired),
      UnaryOp::Negate => info(16, "-", Padding::NotRequired),
      UnaryOp::Not => info(6, "!", Padding::NotRequired),
    }
  }
}

impl OperatorHasInfo for BinaryOp {
  fn op_info(&self) -> OperatorInfo {
    match self {
      BinaryOp::Times => info(15, "*", Padding::Preferred),
      BinaryOp::Div => info(15, "/", Padding::Preferred),
      BinaryOp::Mod => info(15, "%", Padding::Preferred),
      BinaryOp::Add => info(14, "+", Padding::Preferred),
      BinaryOp::Sub => info(13, "-", Padding::Preferred),
      BinaryOp::LShift => info(12, "<<", Padding::Preferred),
      BinaryOp::RShift => info(12, ">>", Padding::Preferred),
      BinaryOp::BitAnd => info(11, "&", Padding::Preferred),
      BinaryOp::BitXor => info(10, "^", Padding::Preferred),
      BinaryOp::BitOr => info(9, "|", Padding::Preferred),
      BinaryOp::LT => info(8, "<", Padding::Preferred),
      BinaryOp::GT => info(8, ">", Padding::Preferred),
      BinaryOp::Eq => info(8, "==", Padding::Preferred),
      BinaryOp::NE => info(8, "!=", Padding::Preferred),
      BinaryOp::LE => info(8, "<=", Padding::Preferred),
      BinaryOp::GE => info(8, ">=", Padding::Preferred),
      BinaryOp::Is => info(18, "is", Padding::Required),
      BinaryOp::In => info(7, "in", Padding::Required),
      BinaryOp::And => info(5, "&&", Padding::Preferred),
      BinaryOp::Or => info(4, "||", Padding::Preferred),
      BinaryOp::Cast => info(2, "as", Padding::Preferred),
    }
  }
}

impl OperatorHasInfo for AssignOp {
  fn op_info(&self) -> OperatorInfo {
    let prec = 1; // All assignments are of the lowest precedence in GDScript
    let pad = Padding::Preferred;
    let name = match self {
      AssignOp::Eq => "=",
      AssignOp::Add => "+=",
      AssignOp::Sub => "-=",
      AssignOp::Times => "*=",
      AssignOp::Div => "/=",
      AssignOp::Mod => "%=",
      AssignOp::BitAnd => "&=",
      AssignOp::BitOr => "|=",
    };
    info(prec, name, pad)
  }
}

impl OperatorHasInfo for TernaryOp {
  fn op_info(&self) -> OperatorInfo {
    info(3, "if-else", Padding::Required)
  }
}
