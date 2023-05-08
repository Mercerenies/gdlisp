
use godot::prelude::*;
use godot::engine::{ScriptVirtual, Script};
use godot::engine::notify::ObjectNotification;

#[derive(Debug, GodotClass)]
#[class(base=Script)]
pub struct GDLispScript {
  #[base]
  base: Base<Script>,
}

#[godot_api]
impl GDLispScript {

}

#[godot_api]
impl ScriptVirtual for GDLispScript {

  fn init(base: Base<Script>) -> Self {
    GDLispScript { base }
  }

  fn to_string(&self) -> GodotString {
    GodotString::from("GDLispScript")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // TODO
  }

}
