// Copyright 2024 Silvio Mayolo
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

use crate::language::GDLispScriptLanguage;
use crate::singleton::GodotSingleton;
use crate::language::loader::GDLispResourceFormatLoader;
use crate::language::saver::GDLispResourceFormatSaver;

use godot::prelude::*;
use godot::init::EditorRunBehavior;

/// Marker struct for the GDLisp Godot extension. This struct will
/// never be instantiated and is simply used as a tag type as the
/// GDExtension entry point for this tool.
struct GDLispExtension;

#[gdextension]
unsafe impl ExtensionLibrary for GDLispExtension {

  fn editor_run_behavior() -> EditorRunBehavior {
    EditorRunBehavior::AllClasses
  }

  fn on_level_init(level: InitLevel) {
    if level == InitLevel::Scene {
      // Initialize GDLispScriptLanguage global singleton.
      GDLispScriptLanguage::initialize_singleton();
      GDLispResourceFormatLoader::initialize_singleton();
      GDLispResourceFormatSaver::initialize_singleton();
    }
  }

  fn on_level_deinit(level: InitLevel) {
    if level == InitLevel::Scene {
      GDLispResourceFormatSaver::deinitialize_singleton();
      GDLispResourceFormatLoader::deinitialize_singleton();
      GDLispScriptLanguage::deinitialize_singleton();
    }
  }

}
