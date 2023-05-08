
use godot::prelude::*;
use godot::engine::Engine;
use godot_core::auto_register_classes;

use crate::language::GDLispScriptLanguage;

struct Main;

struct InitExtensionLayer;

#[gdextension]
unsafe impl ExtensionLibrary for Main {
  fn load_library(handle: &mut InitHandle) -> bool {
    handle.register_layer(InitLevel::Scene, InitExtensionLayer);
    true
  }
}

impl ExtensionLayer for InitExtensionLayer {

  fn initialize(&mut self) {
    auto_register_classes();
    let script_language: Gd<GDLispScriptLanguage> = Gd::new_default();
    Engine::singleton().register_script_language(script_language.upcast());
  }

  fn deinitialize(&mut self) {
    // Currently empty. (TODO De-register the language with the
    // language server)
  }

}
