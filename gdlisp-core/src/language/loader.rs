
use crate::script::GDLispScript;
use gdlisp_util::path::RPathBuf;

use godot::prelude::*;
use godot::engine::{ResourceFormatLoader, IResourceFormatLoader, FileAccess, IScriptExtension};
use godot::engine::file_access::ModeFlags;
use godot::engine::notify::ObjectNotification;

use std::convert::TryFrom;
use std::ops::DerefMut;

#[derive(Debug, GodotClass)]
#[class(base=ResourceFormatLoader)]
pub struct GDLispResourceFormatLoader {
  #[base]
  base: Base<ResourceFormatLoader>,
}

#[godot_api]
impl GDLispResourceFormatLoader {

  pub const CLASS_NAME: &'static str = "GDLispResourceFormatLoader";

}

#[godot_api]
impl IResourceFormatLoader for GDLispResourceFormatLoader {

  fn init(base: Base<ResourceFormatLoader>) -> Self {
    GDLispResourceFormatLoader { base }
  }

  fn to_string(&self) -> GodotString {
    GodotString::from("GDLispResourceFormatSaver")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // Empty method.
  }

  fn get_recognized_extensions(&self) -> PackedStringArray {
    PackedStringArray::from(&[GodotString::from("lisp")])
  }

  fn handles_type(&self, type_: StringName) -> bool {
    return type_ == StringName::from("Script") || type_ == StringName::from("GDLisp")
  }

  fn get_resource_type(&self, path: GodotString) -> GodotString {
    if let Ok(path) = RPathBuf::try_from(path.to_string()) {
      if path.extension().map_or(false, |x| x.to_ascii_lowercase() == "lisp") {
        return GodotString::from("GDLisp");
      }
    }
    GodotString::new()
  }

  fn get_resource_script_class(&self, _path: GodotString) -> GodotString {
    // TODO (Named classes)
    GodotString::new()
  }

  fn load(
    &self,
    path: GodotString,
    original_path: GodotString,
    _use_sub_threads: bool,
    _cache_mode: i32,
  ) -> Variant {
    // TODO Caching
    match FileAccess::open(path, ModeFlags::READ) {
      None => {
        Variant::from(FileAccess::get_open_error())
      }
      Some(file) => {
        let mut resource = Gd::<GDLispScript>::new_default();
        resource.set_source_code(file.get_as_text());
        resource.set_path(original_path);
        Variant::from(resource)
      }
    }
  }

}
