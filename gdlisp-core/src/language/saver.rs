
use crate::script::GDLispScript;
use crate::language::GDLispScriptLanguage;

use godot::prelude::*;
use godot::engine::{ResourceFormatSaver, ResourceFormatSaverVirtual, FileAccess, ScriptLanguageExtensionVirtual, global};
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

}

#[godot_api]
impl ResourceFormatSaverVirtual for GDLispResourceFormatSaver {

  fn init(base: Base<ResourceFormatSaver>) -> Self {
    GDLispResourceFormatSaver { base }
  }

  fn to_string(&self) -> GodotString {
    GodotString::from("GDLispResourceFormatSaver")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // Empty method.
  }

  fn recognize(&self, resource: Gd<Resource>) -> bool {
    resource.try_cast::<GDLispScript>().is_some()
  }

  fn get_recognized_extensions(&self, resource: Gd<Resource>) -> PackedStringArray {
    if self.recognize(resource) {
      PackedStringArray::from(&[GodotString::from("lisp")])
    } else {
      PackedStringArray::new()
    }
  }

  fn save(&mut self, resource: Gd<Resource>, path: GodotString, _flags: i64) -> global::Error {
    match resource.try_cast::<GDLispScript>() {
      None => {
        global::Error::ERR_INVALID_PARAMETER
      }
      Some(resource) => {
        let source = resource.bind().get_source_code();
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
