
use crate::language::GDLispScriptLanguage;
use crate::language::loader::GDLispResourceFormatLoader;
use crate::language::saver::GDLispResourceFormatSaver;

use godot::prelude::*;
use godot::engine::{ResourceLoader, ResourceSaver, Engine};
use godot_core::auto_register_classes;

struct Main;

#[gdextension]
unsafe impl ExtensionLibrary for Main {

  fn on_level_init(level: InitLevel) {
    if (level == InitLevel::Scene) {
      // Initialize GDLispScriptLanguage global singleton.
      GDLispScriptLanguage::init_singleton();
    }
  }

  fn on_level_deinit(level: InitLevel) {
    if (level == InitLevel::Scene) {
      // Nothing to do.
    }
  }

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


      // De-initialize resource loader
      if let Some(loader) = Engine::singleton().get_singleton(StringName::from(GDLispResourceFormatLoader::CLASS_NAME)) {
        ResourceLoader::singleton().remove_resource_format_loader(loader.upcast());
        Engine::singleton().unregister_singleton(StringName::from(GDLispResourceFormatLoader::CLASS_NAME));
      }

      // De-initialize resource saver
      if let Some(saver) = Engine::singleton().get_singleton(StringName::from(GDLispResourceFormatSaver::CLASS_NAME)) {
        ResourceSaver::singleton().remove_resource_format_saver(saver.upcast());
        Engine::singleton().unregister_singleton(StringName::from(GDLispResourceFormatSaver::CLASS_NAME));
      }

    if (level == InitLevel::Scene) {
      // Initialize GDLispScriptLanguage global singleton.
      GDLispScriptLanguage::init_singleton();

      // Initialize resource loader
      let loader: Gd<GDLispResourceFormatLoader> = Gd::new_default();
      Engine::singleton().register_singleton(StringName::from(GDLispResourceFormatLoader::CLASS_NAME), loader.clone().upcast());
      ResourceLoader::singleton().add_resource_format_loader(loader.upcast());

      // Initialize resource saver
      let saver: Gd<GDLispResourceFormatSaver> = Gd::new_default();
      Engine::singleton().register_singleton(StringName::from(GDLispResourceFormatSaver::CLASS_NAME), saver.clone().upcast());
      ResourceSaver::singleton().add_resource_format_saver(saver.upcast());
    }
*/
