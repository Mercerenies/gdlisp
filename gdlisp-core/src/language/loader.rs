
use crate::script::GDLispScript;
use gdlisp_util::path::RPathBuf;

use godot::prelude::*;
use godot::engine::{ResourceLoader, ResourceFormatLoader, IResourceFormatLoader, FileAccess, Engine};
use godot::engine::file_access::ModeFlags;
use godot::engine::notify::ObjectNotification;

use std::convert::TryFrom;

#[derive(Debug, GodotClass)]
#[class(base=ResourceFormatLoader)]
pub struct GDLispResourceFormatLoader {
  #[base]
  base: Base<ResourceFormatLoader>,
}

#[godot_api]
impl GDLispResourceFormatLoader {

  pub const CLASS_NAME: &'static str = "GDLispResourceFormatLoader";

  pub fn initialize_singleton() -> Gd<Self> {
    if let Some(existing_singleton) = Self::get_singleton() {
      return existing_singleton;
    }

    let loader = Self::new_gd();
    Engine::singleton().register_singleton(StringName::from(Self::CLASS_NAME), loader.clone().upcast());
    ResourceLoader::singleton().add_resource_format_loader(loader.clone().upcast());
    loader
  }

  pub fn deinitialize_singleton() {
    if let Some(loader) = Self::get_singleton() {
      ResourceLoader::singleton().remove_resource_format_loader(loader.upcast());
      Engine::singleton().unregister_singleton(StringName::from(Self::CLASS_NAME));
    }
  }

  pub fn get_singleton() -> Option<Gd<Self>> {
    let class_name = StringName::from(Self::CLASS_NAME);
    Engine::singleton()
      .get_singleton(class_name)
      .and_then(|x| x.try_cast().ok())
  }

}

#[godot_api]
impl IResourceFormatLoader for GDLispResourceFormatLoader {

  fn init(base: Base<ResourceFormatLoader>) -> Self {
    GDLispResourceFormatLoader { base }
  }

  fn to_string(&self) -> GString {
    GString::from("GDLispResourceFormatSaver")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // Empty method.
  }

  fn get_recognized_extensions(&self) -> PackedStringArray {
    PackedStringArray::from(&[GString::from("lisp")])
  }

  fn handles_type(&self, type_: StringName) -> bool {
    return type_ == StringName::from("Script") || type_ == StringName::from("GDLisp")
  }

  fn get_resource_type(&self, path: GString) -> GString {
    if let Ok(path) = RPathBuf::try_from(path.to_string()) {
      if path.extension().map_or(false, |x| x.to_ascii_lowercase() == "lisp") {
        return GString::from("GDLisp");
      }
    }
    GString::new()
  }

  fn get_resource_script_class(&self, _path: GString) -> GString {
    // TODO (Named classes)
    GString::new()
  }

  fn load(
    &self,
    path: GString,
    original_path: GString,
    _use_sub_threads: bool,
    _cache_mode: i32,
  ) -> Variant {
    // TODO Caching
    match FileAccess::open(path, ModeFlags::READ) {
      None => {
        Variant::from(FileAccess::get_open_error())
      }
      Some(file) => {
        let mut resource = GDLispScript::new_gd();
        resource.set_source_code(file.get_as_text());
        resource.set_path(original_path);
        Variant::from(resource)
      }
    }
  }

}
