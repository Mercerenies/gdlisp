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

use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct BuiltinClassSizeConfig {
  pub build_configuration: String,
  pub sizes: Vec<PrimitiveTypeSize>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PrimitiveTypeSize {
  pub name: String,
  pub size: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct BuiltinClassOffsetConfig {
  pub build_configuration: String,
  pub classes: Vec<PrimitiveClassOffset>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PrimitiveClassOffset {
  pub name: String,
  pub members: Vec<PrimitiveMemberOffset>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PrimitiveMemberOffset {
  pub member: String,
  pub offset: usize,
}

impl BuiltinClassSizeConfig {

  pub fn size_of(&self, name: &str) -> Option<usize> {
    self.sizes.iter()
      .find(|x| x.name == name)
      .map(|x| x.size)
  }

}
