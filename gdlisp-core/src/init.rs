
use crate::language::GDLispScriptLanguage;
use crate::singleton::GodotSingleton;
use crate::language::loader::GDLispResourceFormatLoader;
use crate::language::saver::GDLispResourceFormatSaver;

use godot::prelude::*;

struct Main;

#[gdextension]
unsafe impl ExtensionLibrary for Main {

  fn on_level_init(level: InitLevel) {
    if level == InitLevel::Scene {
      // Initialize GDLispScriptLanguage global singleton.
      GDLispScriptLanguage::init_singleton();
      GDLispResourceFormatLoader::initialize_singleton();
      GDLispResourceFormatSaver::initialize_singleton();
    }
  }

  fn on_level_deinit(level: InitLevel) {
    if level == InitLevel::Scene {
      GDLispResourceFormatSaver::deinitialize_singleton();
      GDLispResourceFormatLoader::deinitialize_singleton();
    }
  }

}
