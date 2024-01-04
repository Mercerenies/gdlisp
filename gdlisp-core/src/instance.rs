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

use crate::language::GDLispScriptLanguage;
use crate::singleton::GodotSingleton;

use godot::prelude::*;
use godot::engine::{ScriptInstance, Script, ScriptLanguage};
use godot::builtin::meta::{PropertyInfo, MethodInfo};

#[derive(Debug)]
pub struct GDLispScriptInstance {
  // TODO Mocking these so we have somewhere to borrow them from for
  // now :)
  properties: Vec<PropertyInfo>,
  methods: Vec<MethodInfo>,
  owner: Gd<Script>,
}

impl GDLispScriptInstance {

  pub fn new(owner: Gd<Script>) -> GDLispScriptInstance {
    GDLispScriptInstance {
      properties: vec!(),
      methods: vec!(),
      owner,
    }
  }

}

impl ScriptInstance for GDLispScriptInstance {

  fn class_name(&self) -> GString {
    // TODO
    GString::from("")
  }

  fn set(&mut self, _name: StringName, _value: &Variant) -> bool {
    // TODO
    false
  }

  fn get(&self, _name: StringName) -> Option<Variant> {
    // TODO
    None
  }

  fn get_property_list(&self) -> &[PropertyInfo] {
    // TODO
    self.properties.as_slice()
  }

  fn get_method_list(&self) -> &[MethodInfo] {
    // TODO
    self.methods.as_slice()
  }

  // TODO Deal with the recursion problem!
  fn call(&mut self, _method: StringName, _args: &[&Variant]) -> Result<Variant, u32> {
    // TODO
    Ok(Variant::nil())
  }

  fn is_placeholder(&self) -> bool {
    // TODO
    false
  }

  fn has_method(&self, _method: StringName) -> bool {
    // TODO
    false
  }

  fn get_script(&self) -> &Gd<Script> {
    // TODO Can we store this via its actual type?
    &self.owner
  }

  fn get_property_type(&self, _name: StringName) -> VariantType {
    // TODO
    VariantType::Nil
  }

  fn to_string(&self) -> GString {
    GString::from("GDLispScriptInstance")
  }

  fn get_property_state(&self) -> Vec<(StringName, Variant)> {
    // TODO
    vec!()
  }

  fn get_language(&self) -> Gd<ScriptLanguage> {
    GDLispScriptLanguage::initialize_singleton().upcast()
  }

  fn on_refcount_decremented(&self) -> bool {
    // TODO Is this right? I don't think I need any special keep-alive
    // rules.
    true
  }

  fn on_refcount_incremented(&self) {
    // TODO
  }

  fn property_get_fallback(&self, _name: StringName) -> Option<Variant> {
    // TODO
    None
  }

  fn property_set_fallback(&mut self, _name: StringName, _value: &Variant) -> bool {
    // TODO
    false
  }

}
