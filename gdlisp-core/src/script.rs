// Copyright 2024 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

use super::language::GDLispScriptLanguage;
use super::instance::GDLispScriptInstance;
use crate::singleton::GodotSingleton;

use godot::prelude::*;
use godot::engine::{global, IScriptExtension, ScriptExtension, Script, ScriptLanguage, create_script_instance};
use godot::engine::notify::ObjectNotification;

use std::ffi::c_void;

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
    println!("GDLispScript.init");
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
    println!("GDLispScript.editor_can_reload_from_file");
    true
  }

  unsafe fn placeholder_erased(&mut self, _placeholder: *mut c_void) {
    println!("GDLispScript.placeholder_erased");
    // TODO
  }

  fn can_instantiate(&self) -> bool {
    println!("GDLispScript.can_instantiate");
    // TODO
    true
  }

  fn get_base_script(&self) -> Option<Gd<Script>> {
    println!("GDLispScript.get_base_script");
    // TODO
    None
  }

  fn get_global_name(&self) -> StringName {
    println!("GDLispScript.get_global_name");
    // TODO
    StringName::from("")
  }

  fn inherits_script(&self, _script: Gd<Script>) -> bool {
    println!("GDLispScript.inherits_script");
    // TODO
    false
  }

  fn get_instance_base_type(&self) -> StringName {
    println!("GDLispScript.get_instance_base_type");
    // TODO
    StringName::from("")
  }

  unsafe fn instance_create(&self, _for_object: Gd<Object>) -> *mut c_void {
    // TODO
    println!("GDLispScript.instance_create");
    let instance = GDLispScriptInstance::new(self.to_gd().upcast());
    create_script_instance(instance)
  }

  unsafe fn placeholder_instance_create(&self, _for_object: Gd<Object>) -> *mut c_void {
    // TODO (What is the difference between this and regular instance_create?)
    println!("GDLispScript.placeholder_instance_create");
    let instance = GDLispScriptInstance::new(self.to_gd().upcast());
    create_script_instance(instance)
  }

  fn instance_has(&self, _object: Gd<Object>) -> bool {
    println!("GDLispScript.instance_has");
    // TODO
    false
  }

  fn has_source_code(&self) -> bool {
    self.source_code.is_some()
  }

  fn get_source_code(&self) -> GString {
    println!("GDLispScript.get_source_code");
    self.source_code.as_ref().cloned().unwrap_or_default()
  }

  fn set_source_code(&mut self, code: GString) {
    self.source_code = Some(code);
  }

  fn reload(&mut self, _keep_state: bool) -> global::Error {
    // TODO
    println!("GDLispScript.reload");
    global::Error::OK
  }

  fn get_documentation(&self) -> Array<Dictionary> {
    // TODO
    Array::new()
  }

  fn get_class_icon_path(&self) -> GString {
    // TODO
    println!("GDLispScript.get_class_icon_path");
    GString::from("")
  }

  fn has_method(&self, _method: StringName) -> bool {
    // TODO
    println!("GDLispScript.has_method");
    false
  }

  fn has_static_method(&self, _method: StringName) -> bool {
    // TODO
    println!("GDLispScript.has_static_method");
    false
  }

  fn get_method_info(&self, _method: StringName) -> Dictionary {
    // TODO
    Dictionary::new()
  }

  fn is_tool(&self) -> bool {
    // TODO
    println!("GDLispScript.is_tool");
    false
  }

  fn is_valid(&self) -> bool {
    // TODO
    println!("GDLispScript.is_valid");
    true
  }

  fn is_abstract(&self) -> bool {
    // TODO
    println!("GDLispScript.is_abstract");
    true
  }

  fn get_language(&self) -> Option<Gd<ScriptLanguage>> {
    println!("GDLispScript.get_language");
    let language = GDLispScriptLanguage::initialize_singleton();
    Some(language.upcast())
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

  fn setup_local_to_scene(&mut self) {
    // TODO
  }

}
