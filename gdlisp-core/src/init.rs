
use godot::prelude::*;
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
    GDLispScriptLanguage::init_singleton();
  }

  fn deinitialize(&mut self) {
    GDLispScriptLanguage::free_singleton();
  }

}
