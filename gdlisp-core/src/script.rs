
use super::language::GDLispScriptLanguage;

use godot::prelude::*;
use godot::engine::{global, IScriptExtension, ScriptExtension, Script, ScriptLanguage};
use godot::engine::notify::ObjectNotification;

#[derive(Debug, GodotClass)]
#[class(base=ScriptExtension)]
pub struct GDLispScript {
  #[base]
  base: Base<ScriptExtension>,
  source_code: Option<GString>,
}

#[godot_api]
impl GDLispScript {

}

#[godot_api]
impl IScriptExtension for GDLispScript {

  fn init(base: Base<ScriptExtension>) -> Self {
    GDLispScript {
      base,
      source_code: None,
    }
  }

  fn to_string(&self) -> GString {
    GString::from("GDLispScript")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // TODO
  }

  fn editor_can_reload_from_file(&mut self) -> bool {
    // TODO
    true
  }

  fn can_instantiate(&self) -> bool {
    // TODO
    true
  }

  fn get_base_script(&self) -> Option<Gd<Script>> {
    // TODO
    None
  }

  fn get_global_name(&self) -> StringName {
    // TODO
    StringName::from("")
  }

  fn inherits_script(&self, _script: Gd<Script>) -> bool {
    // TODO
    false
  }

  fn get_instance_base_type(&self) -> StringName {
    // TODO
    StringName::from("")
  }

  fn instance_has(&self, _object: Gd<Object>) -> bool {
    // TODO
    false
  }

  fn has_source_code(&self) -> bool {
    // TODO
    true
  }

  fn get_source_code(&self) -> GString {
    self.source_code.as_ref().cloned().unwrap_or_default()
  }

  fn set_source_code(&mut self, code: GString) {
    self.source_code = Some(code);
  }

  fn reload(&mut self, _keep_state: bool) -> global::Error {
    // TODO
    global::Error::OK
  }

  fn get_documentation(&self) -> Array<Dictionary> {
    // TODO
    Array::new()
  }

  fn has_method(&self, _method: StringName) -> bool {
    // TODO
    false
  }

  fn get_method_info(&self, _method: StringName) -> Dictionary {
    // TODO
    Dictionary::new()
  }

  fn is_tool(&self) -> bool {
    // TODO
    false
  }

  fn is_valid(&self) -> bool {
    // TODO
    true
  }

  fn get_language(&self) -> Option<Gd<ScriptLanguage>> {
    Some(GDLispScriptLanguage::singleton().upcast())
  }

  fn has_script_signal(&self, _signal: StringName) -> bool {
    // TODO
    false
  }

  fn get_script_signal_list(&self) -> Array<Dictionary> {
    // TODO
    Array::new()
  }

  fn has_property_default_value(&self, _property: StringName) -> bool {
    // TODO
    false
  }

  fn get_property_default_value(&self, _property: StringName) -> Variant {
    // TODO
    Variant::nil()
  }

  fn update_exports(&mut self) {
    // TODO
  }

  fn get_script_method_list(&self) -> Array<Dictionary> {
    // TODO
    Array::new()
  }

  fn get_script_property_list(&self) -> Array<Dictionary> {
    // TODO
    Array::new()
  }

  fn get_member_line(&self, _member: StringName) -> i32 {
    // TODO
    0
  }

  fn get_constants(&self) -> Dictionary {
    // TODO
    Dictionary::new()
  }

  fn get_members(&self) -> Array<StringName> {
    // TODO
    Array::new()
  }

  fn is_placeholder_fallback_enabled(&self) -> bool {
    // TODO
    false
  }

  fn get_rpc_config(&self) -> Variant {
    // TODO
    Variant::nil()
  }

}
