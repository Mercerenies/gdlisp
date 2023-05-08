
mod singleton;

use super::script::GDLispScript;
use super::template::{ScriptTemplate, TemplateLocation};

use godot::prelude::*;
use godot::engine::{global, ScriptLanguageExtension, ScriptLanguageExtensionVirtual, Script};
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

  /// Initialize the singleton, if it isn't already initialized. Note
  /// that called [`singleton`] will also initialize the singleton,
  /// but this method should be preferred if you don't actually need
  /// the return value right now, to be more explicit.
  pub fn init_singleton() {
    GDLispScriptLanguage::singleton();
  }

  pub fn singleton() -> Gd<GDLispScriptLanguage> {
    singleton::singleton()
  }

  pub fn free_singleton() {
    singleton::free_singleton();
  }

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
    println!("notification");
  }

  fn init_ext(&mut self) {
    // TODO
    println!("init_ext");
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
    println!("reserved");
    PackedStringArray::new() // TODO
  }

  fn is_control_flow_keyword(&self, _keyword: GodotString) -> bool {
    println!("control_flow");
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

  fn make_template(
    &self,
    template: GodotString,
    _class_name: GodotString,
    _base_class_name: GodotString,
  ) -> Option<Gd<Script>> {
    let mut script: Gd<Script> = Gd::<GDLispScript>::new_default().upcast();
    script.set_source_code(template);
    Some(script)
  }

  fn get_built_in_templates(&self, _object: StringName) -> Array<Dictionary> {
    gdlisp_templates()
      .into_iter()
      .map(ScriptTemplate::into_dictionary)
      .collect()
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
    println!("validate");
    Dictionary::new()
  }

  fn validate_path(&self, _path: GodotString) -> GodotString {
    // GDLisp does not place any inherent restrictions on filenames.
    println!("validate path");
    GodotString::from("")
  }

  fn create_script(&self) -> Option<Gd<Object>> {
    println!("create s");
    Some(Gd::<GDLispScript>::new_default().upcast())
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
    println!("can inher");
    true // TODO What is this?
  }

  // Note: The upstream Godot repo seems to have these arguments named
  // incorrectly. This is how they're actually used in Godot.
  fn find_function(&self, _function_name: GodotString, _code: GodotString) -> i64 {
    println!("find fun");
    -1 // TODO
  }

  fn make_function(
    &self,
    _class_name: GodotString,
    _function_name: GodotString,
    _function_args: PackedStringArray,
  ) -> GodotString {
    println!("make fun");
    GodotString::from("") // TODO
  }

  fn open_in_external_editor(
    &mut self,
    _script: Gd<Script>,
    _line: i64,
    _column: i64,
  ) -> global::Error {
    println!("open ext");
    // TODO Consider writing an editor plugin
    global::Error::FAILED
  }

  fn overrides_external_editor(&mut self) -> bool {
    println!("override ext");
    false // TODO
  }

  fn complete_code(
    &self,
    _code: GodotString,
    _path: GodotString,
    _owner: Gd<Object>,
  ) -> Dictionary {
    println!("complete co");
    Dictionary::new() // TODO
  }

  fn lookup_code(
    &self,
    _code: GodotString,
    _symbol: GodotString,
    _path: GodotString,
    _owner: Gd<Object>,
  ) -> Dictionary {
    println!("lookup co");
    Dictionary::new() // TODO
  }

  fn auto_indent_code(&self, code: GodotString, _from_line: i64, _to_line: i64) -> GodotString {
    println!("auto indent");
    code // TODO
  }

  fn add_global_constant(&mut self, _name: StringName, _value: Variant) {
    println!("add glob");
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
    println!("thread enter");
    // TODO
  }

  fn thread_exit(&mut self) {
    println!("thread exit");
    // TODO
  }

  fn debug_get_error(&self) -> GodotString {
    println!("debug get err");
    GodotString::from("") // TODO
  }

  fn debug_get_stack_level_count(&self) -> i64 {
    println!("debug get stack count");
    0 // TODO
  }

  fn debug_get_stack_level_line(&self, _level: i64) -> i64 {
    println!("debug get line");
    0 // TODO
  }

  fn debug_get_stack_level_function(&self, _level: i64) -> GodotString {
    println!("debug get fun");
    GodotString::from("") // TODO
  }

  fn debug_get_stack_level_locals(
    &mut self,
    _level: i64,
    _max_subitems: i64,
    _max_depth: i64,
  ) -> Dictionary {
    println!("debug get loc");
    Dictionary::new() // TODO
  }

  fn debug_get_stack_level_members(
    &mut self,
    _level: i64,
    _max_subitems: i64,
    _max_depth: i64,
  ) -> Dictionary {
    println!("debug get membs");
    Dictionary::new() // TODO
  }

  fn debug_get_globals(&mut self, _max_subitems: i64, _max_depth: i64) -> Dictionary {
    println!("debug get globs");
    Dictionary::new() // TODO
  }

  fn debug_parse_stack_level_expression(
    &mut self,
    _level: i64,
    _expression: GodotString,
    _max_subitems: i64,
    _max_depth: i64,
  ) -> GodotString {
    println!("debug get parse");
    GodotString::from("") // TODO
  }

  fn debug_get_current_stack_info(&mut self) -> Array<Dictionary> {
    println!("debug get stack info");
    Array::new() // TODO
  }

  fn reload_all_scripts(&mut self) {
    println!("reload all");
    // TODO
  }
  fn reload_tool_script(&mut self, _script: Gd<Script>, _soft_reload: bool) {
    println!("reload tool");
    // TODO
  }
  fn get_recognized_extensions(&self) -> PackedStringArray {
    println!("get rec");
    PackedStringArray::from(&[GodotString::from("lisp")])
  }
  fn get_public_functions(&self) -> Array<Dictionary> {
    println!("get publi fun");
    Array::new() // TODO
  }
  fn get_public_constants(&self) -> Dictionary {
    println!("get publi con");
    Dictionary::new() // TODO
  }
  fn get_public_annotations(&self) -> Array<Dictionary> {
    println!("get publi ann");
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
  fn handles_global_class_type(&self, type_: GodotString) -> bool {
    println!("handles");
    type_ == GodotString::from("Script") || type_ == GodotString::from("GDLisp")
  }
  fn get_global_class_name(&self, _path: GodotString) -> Dictionary {
    println!("get global class name");
    Dictionary::new() // TODO
  }

}
