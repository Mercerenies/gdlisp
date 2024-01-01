
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
