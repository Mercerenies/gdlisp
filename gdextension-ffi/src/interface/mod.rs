// Copyright 2023 Silvio Mayolo
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

pub mod data;
pub mod class;

use data::{GodotString, StringName};
use gdextension_api::{ExtensionApi, load_extension_api_from_str};
use crate::internal::godot::{self, GDExtensionInterface, GDExtensionClassLibraryPtr};
use crate::get_build_config;

use std::ffi::{c_void, c_char, CString};

#[derive(Debug)]
pub struct GodotInterface<'a> {
  // We don't technically use this since we preload the functions we
  // need into GodotInterface directly, but it feels very weird to
  // *not* store it here.
  #[allow(unused)]
  interface: &'a GDExtensionInterface,
  library: GDExtensionClassLibraryPtr,
  extension_api: ExtensionApi,
  sizes: Sizes,
  string_new_with_utf8_chars: unsafe extern fn(godot::GDExtensionStringPtr, *const c_char),
  classdb_construct_object: unsafe extern fn(godot::GDExtensionConstStringNamePtr) -> *mut c_void,
  variant_get_ptr_constructor: unsafe extern fn(godot::GDExtensionVariantType, i32) -> godot::GDExtensionPtrConstructor,
  object_set_instance: unsafe extern fn(godot::GDExtensionObjectPtr, godot::GDExtensionConstStringNamePtr, godot::GDExtensionClassInstancePtr),
  classdb_register_extension_class: unsafe extern fn(godot::GDExtensionClassLibraryPtr, godot::GDExtensionConstStringNamePtr, godot::GDExtensionConstStringNamePtr, *const godot::GDExtensionClassCreationInfo),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Sizes {
  pub string: usize,
  pub string_name: usize,
}

static JSON_EXTENSION_DATA: &'static str = include_str!("../../extension_api.json");

impl<'a> GodotInterface<'a> {

  pub fn new(interface: &'a GDExtensionInterface, library: GDExtensionClassLibraryPtr) -> Self {
    // Go ahead and extract pointers to all of the library functions
    // we need. If any are null for some reason, we catch it now
    // rather than later.
    let string_new_with_utf8_chars = interface.string_new_with_utf8_chars.unwrap();
    let classdb_construct_object = interface.classdb_construct_object.unwrap();
    let variant_get_ptr_constructor = interface.variant_get_ptr_constructor.unwrap();
    let object_set_instance = interface.object_set_instance.unwrap();
    let classdb_register_extension_class = interface.classdb_register_extension_class.unwrap();

    // Load the extension API from the JSON file.
    let extension_api = load_extension_api_from_str(JSON_EXTENSION_DATA).unwrap();
    let sizes = Sizes::new(&extension_api);

    Self {
      interface,
      library,
      extension_api,
      sizes,
      string_new_with_utf8_chars,
      classdb_construct_object,
      variant_get_ptr_constructor,
      object_set_instance,
      classdb_register_extension_class,
    }
  }

  pub fn string(&self, s: &str) -> GodotString {
    let cstr = CString::new(s).unwrap();
    let mut opaque = alloc(self.sizes.string);
    unsafe {
      (self.string_new_with_utf8_chars)(opaque.as_mut_ptr() as *mut c_void, cstr.as_ptr());
    }
    GodotString { opaque }
  }

  pub fn string_name(&self, s: &str) -> StringName {
    let godot_str = self.string(s);
    let string_name_ctor_idx = get_string_to_string_name_ctor(&self.extension_api).unwrap();
    let string_name_ctor = unsafe {
      (self.variant_get_ptr_constructor)(godot::GDExtensionVariantType_GDEXTENSION_VARIANT_TYPE_STRING_NAME, string_name_ctor_idx as i32).unwrap()
    };

    let mut opaque = alloc(self.sizes.string_name);
    let mut arr = [godot_str.opaque.as_ptr() as *const c_void];
    unsafe {
      string_name_ctor(opaque.as_mut_ptr() as *mut c_void, arr.as_mut_ptr());
    }

    StringName { opaque }
  }

  pub fn register_class(&mut self, class: impl class::GodotExtensionClass + 'static) {
    let class_name = self.string_name(class.class_name());
    let parent_class_name = self.string_name(class.parent_class_name());
    let is_virtual = class.is_virtual() as u8;
    let is_abstract = class.is_abstract() as u8;

    let userdata = Box::into_raw(
      Box::new(class::Userdata { interface: self, class: Box::new(class) })
    );

    let info = godot::GDExtensionClassCreationInfo {
      is_virtual: is_virtual,
      is_abstract: is_abstract,
      set_func: Some(class::set_func_bind),
      get_func: Some(class::get_func_bind),
      get_property_list_func: Some(class::get_property_list_func_bind),
      free_property_list_func: Some(class::free_property_list_func_bind),
      property_can_revert_func: Some(class::property_can_revert_func_bind),
      property_get_revert_func: Some(class::property_get_revert_func_bind),
      notification_func: Some(class::notification_func_bind),
      to_string_func: Some(class::to_string_func_bind),
      reference_func: None,
      unreference_func: None,
      create_instance_func: Some(class::create_instance_func_bind),
      free_instance_func: Some(class::free_instance_func_bind),
      get_virtual_func: Some(class::get_virtual_func_bind),
      get_rid_func: None,
      class_userdata: userdata as *mut c_void,
    };

    unsafe {
      (self.classdb_register_extension_class)(
        self.library,
        class_name.opaque.as_ptr() as *const c_void,
        parent_class_name.opaque.as_ptr() as *const c_void,
        &info,
      );
    }
  }

}

fn get_string_to_string_name_ctor(api: &ExtensionApi) -> Option<u32> {
  api.builtin_classes
    .iter()
    .find(|cls| cls.name == "StringName")
    .and_then(|cls| {
      cls.constructors
        .iter()
        .find(|ctor| ctor.arguments.len() == 1 && ctor.arguments[0].arg_type == "String")
        .map(|ctor| ctor.index)
    })
}

fn alloc(size: usize) -> Vec<u8> {
  vec![0; size]
}

impl Sizes {

  fn new(extension_api: &ExtensionApi) -> Sizes {
    // Panics if the build config is invalid or String does not exist
    // in the API.
    let builtin_class_sizes = extension_api.get_builtin_class_size_config(get_build_config()).unwrap();
    Sizes {
      string: builtin_class_sizes.size_of("String").unwrap(),
      string_name: builtin_class_sizes.size_of("StringName").unwrap(),
    }
  }

}
