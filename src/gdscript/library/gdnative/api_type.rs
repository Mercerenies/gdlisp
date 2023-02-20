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

//! The [`ApiType`] enum, indicating whether a GDScript class is in
//! the core library or intended for editor tooling.

use serde::{Serialize, Deserialize};

/// The type of API to which a GDScript built-in class belongs.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ApiType {
  /// A core class, which is always available.
  Core,
  /// A tooling class, available in Godot builds which have access to
  /// the editor.
  Tools,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn serialize_api_type_test() {
    let core = serde_json::to_string(&ApiType::Core).unwrap();
    assert_eq!(core, "\"core\"");
    let tools = serde_json::to_string(&ApiType::Tools).unwrap();
    assert_eq!(tools, "\"tools\"");
  }

  #[test]
  fn deserialize_api_type_test() {
    let core: ApiType = serde_json::from_str("\"core\"").unwrap();
    assert_eq!(core, ApiType::Core);
    let tools: ApiType = serde_json::from_str("\"tools\"").unwrap();
    assert_eq!(tools, ApiType::Tools);
  }

}
