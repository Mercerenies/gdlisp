
use crate::init::{InitObject, ExtensionInitializer};
use crate::init::level::InitializationLevel;
use crate::interface::GodotInterface;
use crate::interface::class::GodotExtensionClass;
use crate::internal::godot;

struct GDLispExtensionInitializer;

struct GDLispExtensionClass;

const GDLISP_CLASS_NAME: &str = "GDLisp";

#[no_mangle] pub unsafe extern fn gdlisp_extension_init(p_interface: *const godot::GDExtensionInterface, p_library: godot::GDExtensionClassLibraryPtr, r_initialization: *mut godot::GDExtensionInitialization) -> godot::GDExtensionBool {
  let init = InitObject::from_ptr(p_interface, p_library);
  init.setup_init(GDLispExtensionInitializer, &mut *r_initialization, InitializationLevel::Scene);
  1
}

impl ExtensionInitializer for GDLispExtensionInitializer {

  fn initialize_level(&mut self, interface: &mut GodotInterface<'_>, level: InitializationLevel) {
    if level == InitializationLevel::Scene {
      interface.register_class(GDLispExtensionClass)
    }
  }

  fn deinitialize_level(&mut self, interface: &mut GodotInterface<'_>, level: InitializationLevel) {
    if level == InitializationLevel::Scene {
      interface.unregister_class(GDLISP_CLASS_NAME);
    }
  }

}

impl GodotExtensionClass for GDLispExtensionClass {

  fn class_name(&self) -> &str {
    GDLISP_CLASS_NAME
  }

  fn parent_class_name(&self) -> &str {
    "ScriptLanguageExtension"
  }

  fn is_virtual(&self) -> bool {
    false
  }

  fn is_abstract(&self) -> bool {
    false
  }

  fn to_string(&self) -> String {
    String::from("#<GDLisp>")
  }

}
