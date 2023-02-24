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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum InitializationLevel {
  Core, Servers, Scene, Editor,
}

pub const MAX_INITIALIZATION_LEVEL: u32 = 4;

impl InitializationLevel {

  pub fn into_u32(self) -> u32 {
    self as u32
  }

  pub fn try_from_u32(n: u32) -> Option<Self> {
    match n {
      0 => Some(InitializationLevel::Core),
      1 => Some(InitializationLevel::Servers),
      2 => Some(InitializationLevel::Scene),
      3 => Some(InitializationLevel::Editor),
      _ => None,
    }
  }

  pub fn from_u32(n: u32) -> Self {
    InitializationLevel::try_from_u32(n)
      .unwrap_or_else(|| panic!("Invalid u32 {} for InitializationLevel::from_u32", n))
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::internal::godot;

  #[test]
  fn test_init_levels_match_godot() {
    assert_eq!(InitializationLevel::Core.into_u32(), godot::GDExtensionInitializationLevel_GDEXTENSION_INITIALIZATION_CORE);
    assert_eq!(InitializationLevel::Servers.into_u32(), godot::GDExtensionInitializationLevel_GDEXTENSION_INITIALIZATION_SERVERS);
    assert_eq!(InitializationLevel::Scene.into_u32(), godot::GDExtensionInitializationLevel_GDEXTENSION_INITIALIZATION_SCENE);
    assert_eq!(InitializationLevel::Editor.into_u32(), godot::GDExtensionInitializationLevel_GDEXTENSION_INITIALIZATION_EDITOR);
    assert_eq!(MAX_INITIALIZATION_LEVEL, godot::GDExtensionInitializationLevel_GDEXTENSION_MAX_INITIALIZATION_LEVEL);
  }

  #[test]
  fn test_roundtrip() {
    for i in 0..MAX_INITIALIZATION_LEVEL {
      assert_eq!(InitializationLevel::from_u32(i).into_u32(), i);
    }
  }

}
