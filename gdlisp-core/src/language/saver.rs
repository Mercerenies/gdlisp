
use crate::script::GDLispScript;
use crate::singleton::GodotSingleton;
use crate::language::GDLispScriptLanguage;

use godot::prelude::*;
use godot::engine::{ResourceFormatSaver, IResourceFormatSaver, FileAccess,
                    IScriptLanguageExtension, global, ResourceSaver};
use godot::engine::file_access::ModeFlags;
use godot::engine::notify::ObjectNotification;

#[derive(Debug, GodotClass)]
#[class(base=ResourceFormatSaver)]
pub struct GDLispResourceFormatSaver {
  #[base]
  base: Base<ResourceFormatSaver>,
}

#[godot_api]
impl GDLispResourceFormatSaver {}

impl GodotSingleton for GDLispResourceFormatSaver {
  const CLASS_NAME: &'static str = "GDLispResourceFormatSaver";

  fn allocate_singleton() -> Gd<Self> {
    let instance = Self::new_gd();
    ResourceSaver::singleton().add_resource_format_saver(instance.clone().upcast());
    instance
  }

  fn deallocate_singleton(instance: Gd<Self>) {
    ResourceSaver::singleton().remove_resource_format_saver(instance.upcast());
    // No need to explicitly free, as GDLispResourceFormatSaver is
    // RefCounted.
  }
}

#[godot_api]
impl IResourceFormatSaver for GDLispResourceFormatSaver {

  fn init(base: Base<ResourceFormatSaver>) -> Self {
    GDLispResourceFormatSaver { base }
  }

  fn to_string(&self) -> GString {
    GString::from("GDLispResourceFormatSaver")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // Empty method.
  }

  fn recognize(&self, resource: Gd<Resource>) -> bool {
    resource.try_cast::<GDLispScript>().is_ok()
  }

  fn get_recognized_extensions(&self, resource: Gd<Resource>) -> PackedStringArray {
    if self.recognize(resource) {
      PackedStringArray::from(&[GString::from("lisp")])
    } else {
      PackedStringArray::new()
    }
  }

  fn save(&mut self, resource: Gd<Resource>, path: GString, _flags: u32) -> global::Error {
    match resource.try_cast::<GDLispScript>() {
      Err(_) => {
        global::Error::ERR_INVALID_PARAMETER
      }
      Ok(resource) => {
        let source = resource.get_source_code();
        match FileAccess::open(path, ModeFlags::WRITE) {
          None => {
            FileAccess::get_open_error()
          }
          Some(mut file) => {
            file.store_string(source);
            let err = file.get_error();
            if err != global::Error::OK && err != global::Error::ERR_FILE_EOF {
              return err;
            }
            let mut language = GDLispScriptLanguage::initialize_singleton();
            language.bind_mut().reload_tool_script(resource.upcast(), false);
            global::Error::OK
          }
        }
      }
    }
  }

}