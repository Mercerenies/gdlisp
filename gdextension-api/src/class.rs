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

use crate::function::Argument;
use crate::constant::{BuiltinClassConstant, ClassConstant};
use crate::godot_enum::Enum;

use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct BuiltinClass {
  pub name: String,
  pub is_keyed: bool,
  pub indexing_return_type: Option<String>,
  pub operators: Vec<Operator>,
  pub constructors: Vec<Constructor>,
  #[serde(default)]
  pub constants: Vec<BuiltinClassConstant>,
  #[serde(default)]
  pub enums: Vec<Enum>,
  #[serde(default)]
  pub methods: Vec<Method>,
  #[serde(default)]
  pub members: Vec<Argument>,
  pub has_destructor: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Class {
  pub name: String,
  pub is_refcounted: bool,
  pub is_instantiable: bool,
  pub inherits: Option<String>,
  pub api_type: ApiType,
  #[serde(default)]
  pub constants: Vec<ClassConstant>,
  #[serde(default)]
  pub enums: Vec<Enum>,
  #[serde(default)]
  pub methods: Vec<Method>,
  #[serde(default)]
  pub members: Vec<Argument>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ApiType {
  Core,
  Editor,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Operator {
  pub name: OperatorName,
  pub right_type: Option<String>,
  pub return_type: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Constructor {
  pub index: u32,
  #[serde(default)]
  pub arguments: Vec<Argument>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Method {
  pub name: String,
  pub return_type: Option<String>,
  pub is_vararg: bool,
  pub is_const: bool,
  pub is_static: bool,
  pub hash: Option<i64>,
  #[serde(default)]
  pub arguments: Vec<Argument>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum OperatorName {
  #[serde(rename = "==")]
  Equal,
  #[serde(rename = "!=")]
  NotEqual,
  #[serde(rename = "<")]
  LessThan,
  #[serde(rename = ">")]
  GreaterThan,
  #[serde(rename = "<=")]
  LessThanOREqual,
  #[serde(rename = ">=")]
  GreaterThanOrEqual,
  #[serde(rename = "unary-")]
  UnaryMinus,
  #[serde(rename = "unary+")]
  UnaryPlus,
  #[serde(rename = "~")]
  Complement,
  #[serde(rename = "+")]
  Plus,
  #[serde(rename = "-")]
  Minus,
  #[serde(rename = "*")]
  Times,
  #[serde(rename = "/")]
  Divide,
  #[serde(rename = "%")]
  Modulo,
  #[serde(rename = "**")]
  Exponent,
  #[serde(rename = "<<")]
  LeftShift,
  #[serde(rename = ">>")]
  RightShift,
  #[serde(rename = "&")]
  BitAnd,
  #[serde(rename = "|")]
  BitOr,
  #[serde(rename = "^")]
  BitXor,
  Or,
  Not,
  And,
  Xor,
  In,
}
