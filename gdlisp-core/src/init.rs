
use godot::prelude::*;
use godot::engine::Engine;
use godot_core::auto_register_classes;

use crate::language::GDLispScriptLanguage;

struct Main;

#[derive(Default)]
struct InitExtensionLayer {
  gdlisp_language: Option<Gd<GDLispScriptLanguage>>,
}

impl InitExtensionLayer {
  pub fn new() -> InitExtensionLayer {
    Self::default()
  }
}

#[gdextension]
unsafe impl ExtensionLibrary for Main {
  fn load_library(handle: &mut InitHandle) -> bool {
    handle.register_layer(InitLevel::Scene, InitExtensionLayer::new());
    true
  }
}

impl ExtensionLayer for InitExtensionLayer {

  fn initialize(&mut self) {
    auto_register_classes();
    let script_language: Gd<GDLispScriptLanguage> = Gd::new_default();
    self.gdlisp_language = Some(script_language.share());
    Engine::singleton().register_script_language(script_language.upcast());
  }

  fn deinitialize(&mut self) {
    if let Some(script_language) = self.gdlisp_language.take() {
      Engine::singleton().unregister_script_language(script_language.share().upcast());
      script_language.free();
    }
  }

}
