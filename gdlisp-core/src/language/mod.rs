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

pub mod loader;
pub mod saver;

use crate::script::GDLispScript;
use crate::singleton::GodotSingleton;
use crate::types::template::{ScriptTemplate, TemplateLocation};
use crate::types::validate::Validation;

use godot::prelude::*;
use godot::engine::{global, ScriptLanguageExtension, IScriptLanguageExtension, Script, Engine};
use godot::engine::notify::ObjectNotification;

#[derive(Debug, GodotClass)]
#[class(base=ScriptLanguageExtension)]
pub struct GDLispScriptLanguage {
  #[base]
  base: Base<ScriptLanguageExtension>,
}

fn gdlisp_templates() -> impl IntoIterator<Item=ScriptTemplate> {
  [
    ScriptTemplate {
      inherit: String::from("Object"),
      name: String::from("Empty"),
      description: String::from("Empty template suitable for all Objects"),
      content: String::from(""), // TODO
      id: 0,
      origin: TemplateLocation::BuiltIn,
    }
  ]
}

#[godot_api]
impl GDLispScriptLanguage {

}

impl GodotSingleton for GDLispScriptLanguage {
  const CLASS_NAME: &'static str = "GDLispScriptLanguage";

  fn allocate_singleton() -> Gd<Self> {
    let script_language = GDLispScriptLanguage::alloc_gd();
    Engine::singleton().register_script_language(script_language.clone().upcast());
    script_language
  }

  fn deallocate_singleton(instance: Gd<Self>) {
    Engine::singleton().unregister_script_language(instance.clone().upcast());
    instance.free();
  }

}

#[godot_api]
impl IScriptLanguageExtension for GDLispScriptLanguage {

  fn init(base: Base<ScriptLanguageExtension>) -> Self {
    GDLispScriptLanguage { base }
  }

  fn to_string(&self) -> GString {
    GString::from("GDLisp")
  }

  fn get_name(&self) -> GString {
    GString::from("GDLisp")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // TODO
    println!("GDLispScriptLanguage.on_notification");
  }

  fn init_ext(&mut self) {
    // TODO
    println!("GDLispScriptLanguage.init_ext");
  }

  fn get_type(&self) -> GString {
    GString::from("GDLisp")
  }

  fn get_extension(&self) -> GString {
    GString::from("lisp")
  }

  fn finish(&mut self) {
    // TODO
    println!("GDLispScriptLanguage.finish");
  }

  fn get_reserved_words(&self) -> PackedStringArray {
    println!("GDLispScriptLanguage.get_reserved_words");
    PackedStringArray::new() // TODO
  }

  fn is_control_flow_keyword(&self, _keyword: GString) -> bool {
    println!("GDLispScriptLanguage.is_control_flow_keyword");
    false // TODO
  }

  fn get_comment_delimiters(&self) -> PackedStringArray {
    PackedStringArray::from(&[
      GString::from(";"), // Line comments
      GString::from("#| |#"), // Block comments
    ])
  }

  fn get_string_delimiters(&self) -> PackedStringArray {
    PackedStringArray::from(&[
      GString::from("\" \""),
    ])
  }

  fn make_template(
    &self,
    template: GString,
    _class_name: GString,
    _base_class_name: GString,
  ) -> Option<Gd<Script>> {
    println!("GDLispScriptLanguage.make_template");
    let mut script = GDLispScript::new_gd();
    script.set_source_code(template);
    Some(script.upcast())
  }

  fn get_built_in_templates(&self, object: StringName) -> Array<Dictionary> {
    gdlisp_templates()
      .into_iter()
      .filter(|x| StringName::from(&x.inherit) == object)
      .map(ScriptTemplate::into_dictionary)
      .collect()
  }

  fn is_using_templates(&mut self) -> bool {
    true
  }

  fn validate(
    &self,
    _script: GString,
    _path: GString,
    _validate_functions: bool,
    _validate_errors: bool,
    _validate_warnings: bool,
    _validate_safe_lines: bool,
  ) -> Dictionary {
    // TODO This for real
    println!("GDLispScriptLanguage.validate");
    let validation = Validation {
      functions: vec![],
      errors: vec![],
      warnings: vec![],
      safe_lines: vec![],
      valid: true,
    };
    validation.into_dictionary()
  }

  fn validate_path(&self, _path: GString) -> GString {
    // GDLisp does not place any inherent restrictions on filenames.
    GString::from("")
  }

  fn create_script(&self) -> Option<Gd<Object>> {
    println!("GDLispScriptLanguage.create_script");
    Some(GDLispScript::new_gd().upcast())
  }

  fn has_named_classes(&self) -> bool {
    false
  }

  fn supports_builtin_mode(&self) -> bool {
    true
  }

  fn supports_documentation(&self) -> bool {
    true
  }

  fn can_inherit_from_file(&self) -> bool {
    println!("GDLispScriptLanguage.can_inherit_from_file");
    true // TODO What is this?
  }

  // Note: The upstream Godot repo seems to have these arguments named
  // incorrectly. This is how they're actually used in Godot.
  fn find_function(&self, _function_name: GString, _code: GString) -> i32 {
    println!("GDLispScriptLanguage.find_function");
    -1 // TODO
  }

  fn make_function(
    &self,
    _class_name: GString,
    _function_name: GString,
    _function_args: PackedStringArray,
  ) -> GString {
    println!("GDLispScriptLanguage.make_function");
    GString::from("") // TODO
  }

  fn open_in_external_editor(
    &mut self,
    _script: Gd<Script>,
    _line: i32,
    _column: i32,
  ) -> global::Error {
    println!("open ext");
    // TODO Consider writing an editor plugin
    global::Error::FAILED
  }

  fn overrides_external_editor(&mut self) -> bool {
    println!("GDLispScriptLanguage.overrides_external_editor");
    false // TODO
  }

  fn complete_code(
    &self,
    _code: GString,
    _path: GString,
    _owner: Gd<Object>,
  ) -> Dictionary {
    println!("GDLispScriptLanguage.complete_code");
    Dictionary::new() // TODO
  }

  fn lookup_code(
    &self,
    _code: GString,
    _symbol: GString,
    _path: GString,
    _owner: Gd<Object>,
  ) -> Dictionary {
    println!("GDLispScriptLanguage.lookup_code");
    Dictionary::new() // TODO
  }

  fn auto_indent_code(&self, code: GString, _from_line: i32, _to_line: i32) -> GString {
    println!("GDLispScriptLanguage.auto_indent_code");
    code // TODO
  }

  fn add_global_constant(&mut self, _name: StringName, _value: Variant) {
    println!("GDLispScriptLanguage.add_global_constant");
    // TODO
  }

  fn add_named_global_constant(&mut self, _name: StringName, _value: Variant) {
    println!("add named glob");
    // TODO
  }

  fn remove_named_global_constant(&mut self, _name: StringName) {
    println!("remo named glob");
    // TODO
  }

  fn thread_enter(&mut self) {
    println!("GDLispScriptLanguage.thread_enter");
    // TODO
  }

  fn thread_exit(&mut self) {
    println!("GDLispScriptLanguage.thread_exit");
    // TODO
  }

  fn debug_get_error(&self) -> GString {
    println!("debug get err");
    GString::from("") // TODO
  }

  fn debug_get_stack_level_count(&self) -> i32 {
    println!("debug get stack count");
    0 // TODO
  }

  fn debug_get_stack_level_line(&self, _level: i32) -> i32 {
    println!("debug get line");
    0 // TODO
  }

  fn debug_get_stack_level_function(&self, _level: i32) -> GString {
    println!("debug get fun");
    GString::from("") // TODO
  }

  fn debug_get_stack_level_locals(
    &mut self,
    _level: i32,
    _max_subitems: i32,
    _max_depth: i32,
  ) -> Dictionary {
    println!("debug get loc");
    Dictionary::new() // TODO
  }

  fn debug_get_stack_level_members(
    &mut self,
    _level: i32,
    _max_subitems: i32,
    _max_depth: i32,
  ) -> Dictionary {
    println!("debug get membs");
    Dictionary::new() // TODO
  }

  fn debug_get_globals(&mut self, _max_subitems: i32, _max_depth: i32) -> Dictionary {
    println!("debug get globs");
    Dictionary::new() // TODO
  }

  fn debug_parse_stack_level_expression(
    &mut self,
    _level: i32,
    _expression: GString,
    _max_subitems: i32,
    _max_depth: i32,
  ) -> GString {
    println!("debug get parse");
    GString::from("") // TODO
  }

  fn debug_get_current_stack_info(&mut self) -> Array<Dictionary> {
    println!("debug get stack info");
    Array::new() // TODO
  }

  fn reload_all_scripts(&mut self) {
    println!("GDLispScriptLanguage.reload_all_scripts");
    // TODO
  }
  fn reload_tool_script(&mut self, _script: Gd<Script>, _soft_reload: bool) {
    println!("GDLispScriptLanguage.reload_tool_script");
    // TODO
  }
  fn get_recognized_extensions(&self) -> PackedStringArray {
    println!("GDLispScriptLanguage.get_recognized_extensions");
    PackedStringArray::from(&[GString::from("lisp")])
  }
  fn get_public_functions(&self) -> Array<Dictionary> {
    println!("GDLispScriptLanguage.get_public_functions");
    Array::new() // TODO
  }
  fn get_public_constants(&self) -> Dictionary {
    println!("GDLispScriptLanguage.get_public_constants");
    Dictionary::new() // TODO
  }
  fn get_public_annotations(&self) -> Array<Dictionary> {
    println!("GDLispScriptLanguage.get_public_annotations");
    Array::new() // TODO
  }
  fn profiling_start(&mut self) {
    println!("prof start");
    // TODO
  }
  fn profiling_stop(&mut self) {
    println!("prof stop");
    // TODO
  }
  fn frame(&mut self) {
    // TODO
  }
  fn handles_global_class_type(&self, type_: GString) -> bool {
    println!("handles");
    type_ == GString::from("GDLisp")
  }
  fn get_global_class_name(&self, _path: GString) -> Dictionary {
    println!("get global class name");
    Dictionary::new() // TODO
  }

}
