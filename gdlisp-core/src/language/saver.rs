
use crate::script::GDLispScript;
use crate::language::GDLispScriptLanguage;

use godot::prelude::*;
use godot::engine::{ResourceFormatSaver, IResourceFormatSaver, FileAccess,
                    IScriptLanguageExtension, global, Engine, ResourceSaver};
use godot::engine::file_access::ModeFlags;
use godot::engine::notify::ObjectNotification;

#[derive(Debug, GodotClass)]
#[class(base=ResourceFormatSaver)]
pub struct GDLispResourceFormatSaver {
  #[base]
  base: Base<ResourceFormatSaver>,
}

#[godot_api]
impl GDLispResourceFormatSaver {

  pub const CLASS_NAME: &'static str = "GDLispResourceFormatSaver";

  pub fn initialize_singleton() -> Gd<Self> {
    if let Some(existing_singleton) = Self::get_singleton() {
      return existing_singleton;
    }

    let saver = Self::new_gd();
    Engine::singleton().register_singleton(StringName::from(Self::CLASS_NAME), saver.clone().upcast());
    ResourceSaver::singleton().add_resource_format_saver(saver.clone().upcast());
    saver
  }

  pub fn deinitialize_singleton() {
    if let Some(saver) = Self::get_singleton() {
      ResourceSaver::singleton().remove_resource_format_saver(saver.upcast());
      Engine::singleton().unregister_singleton(StringName::from(Self::CLASS_NAME));
    }
  }

  pub fn get_singleton() -> Option<Gd<Self>> {
    let class_name = StringName::from(Self::CLASS_NAME);
    if Engine::singleton().has_singleton(class_name.clone()) {
      let singleton = Engine::singleton().get_singleton(class_name).expect("Singleton in inconsistent state");
      singleton.try_cast().ok()
    } else {
      None
    }
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
            GDLispScriptLanguage::singleton().bind_mut().reload_tool_script(resource.upcast(), false);
            global::Error::OK
          }
        }
      }
    }
  }

}
