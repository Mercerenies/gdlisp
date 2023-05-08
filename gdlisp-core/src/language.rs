
use godot::prelude::*;
use godot::engine::{ScriptLanguageExtension, ScriptLanguageExtensionVirtual, Script};
use godot::engine::notify::ObjectNotification;

#[derive(Debug, GodotClass)]
#[class(base=ScriptLanguageExtension)]
pub struct GDLispScriptLanguage {
  #[base]
  base: Base<ScriptLanguageExtension>,
}

#[godot_api]
impl GDLispScriptLanguage {

}

#[godot_api]
impl ScriptLanguageExtensionVirtual for GDLispScriptLanguage {

  fn init(base: Base<ScriptLanguageExtension>) -> Self {
    GDLispScriptLanguage { base }
  }

  fn to_string(&self) -> GodotString {
    GodotString::from("GDLisp")
  }

  fn get_name(&self) -> GodotString {
    GodotString::from("GDLisp")
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // TODO
  }

  fn init_ext(&mut self) {
    // TODO
  }

  fn get_type(&self) -> GodotString {
    GodotString::from("GDLisp")
  }

  fn get_extension(&self) -> GodotString {
    GodotString::from("lisp")
  }

  fn finish(&mut self) {
    // TODO
  }

  fn get_reserved_words(&self) -> PackedStringArray {
    PackedStringArray::new() // TODO
  }

  fn is_control_flow_keyword(&self, _keyword: GodotString) -> bool {
    false // TODO
  }

  fn get_comment_delimiters(&self) -> PackedStringArray {
    PackedStringArray::from(&[
      GodotString::from(";"), // Line comments
      GodotString::from("#| |#"), // Block comments
    ])
  }

  fn get_string_delimiters(&self) -> PackedStringArray {
    PackedStringArray::from(&[
      GodotString::from("\" \""),
    ])
  }

  /*
  fn make_template(
    &self,
    _template: GodotString,
    _class_name: GodotString,
    _base_class_name: GodotString,
  ) -> Option<Gd<Script>> {
    None // TODO (/////)
  }
   */

  fn get_built_in_templates(&self, _object: StringName) -> Array<Dictionary> {
    Array::new() // TODO
  }

  fn is_using_templates(&mut self) -> bool {
    true
  }

  fn validate(
    &self,
    _script: GodotString,
    _path: GodotString,
    _validate_functions: bool,
    _validate_errors: bool,
    _validate_warnings: bool,
    _validate_safe_lines: bool,
  ) -> Dictionary {
    Dictionary::new()
  }

  fn validate_path(&self, _path: GodotString) -> GodotString {
    // GDLisp does not place any inherent restrictions on filenames.
    GodotString::from("")
  }

  /*
  fn create_script(&self) -> Option<Gd<Object>> {
    
  }
   */

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
    true // TODO What is this?
  }

  /////

}
