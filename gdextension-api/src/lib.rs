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

pub mod build_config;
pub mod class;
pub mod constant;
pub mod function;
pub mod godot_enum;
pub mod header;
pub mod native_structure;
pub mod singleton;

use serde::{Serialize, Deserialize};

use std::io::Read;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ExtensionApi {
  pub header: header::Header,
  pub builtin_class_sizes: Vec<build_config::BuiltinClassSizeConfig>,
  pub global_constants: Vec<constant::Constant>,
  pub global_enums: Vec<godot_enum::Enum>,
  pub utility_functions: Vec<function::UtilityFunction>,
  pub builtin_classes: Vec<class::BuiltinClass>,
  pub classes: Vec<class::Class>,
  pub singletons: Vec<singleton::Singleton>,
  pub native_structures: Vec<native_structure::NativeStructure>,
}

pub fn load_extension_api(rdr: impl Read) -> Result<ExtensionApi, serde_json::Error> {
  serde_json::from_reader(rdr)
}

pub fn load_extension_api_from_str(s: &str) -> Result<ExtensionApi, serde_json::Error> {
  serde_json::from_str(s)
}
