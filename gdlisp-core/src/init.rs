
use crate::language::GDLispScriptLanguage;
use crate::language::loader::GDLispResourceFormatLoader;
use crate::language::saver::GDLispResourceFormatSaver;

use godot::prelude::*;
use godot::engine::{ResourceLoader, ResourceSaver};
use godot_core::auto_register_classes;

struct Main;

#[gdextension]
unsafe impl ExtensionLibrary for Main {

/*
  fn load_library(handle: &mut InitHandle) -> bool {
    handle.register_layer(InitLevel::Scene, InitExtensionLayer::default());
    true
  }
*/
}

/*
impl ExtensionLayer for InitExtensionLayer {

  fn initialize(&mut self) {
    auto_register_classes();

    let loader: Gd<GDLispResourceFormatLoader> = Gd::new_default();
    let saver: Gd<GDLispResourceFormatSaver> = Gd::new_default();
    self.loader = Some(loader.share());
    self.saver = Some(saver.share());

    GDLispScriptLanguage::init_singleton();
    ResourceLoader::singleton().add_resource_format_loader(loader.upcast(), false);
    ResourceSaver::singleton().add_resource_format_saver(saver.upcast(), false);
  }

  fn deinitialize(&mut self) {
    if let Some(loader) = self.loader.take() {
      ResourceLoader::singleton().remove_resource_format_loader(loader.upcast());
    }
    if let Some(saver) = self.saver.take() {
      ResourceSaver::singleton().remove_resource_format_saver(saver.upcast());
    }

    GDLispScriptLanguage::free_singleton();
  }

}
*/
