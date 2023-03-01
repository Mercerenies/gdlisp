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

use crate::internal::godot;
use super::GodotInterface;

use std::ffi::{c_void, CString};
use std::ptr;

pub trait GodotExtensionClass {

  fn class_name(&self) -> &str;

  fn parent_class_name(&self) -> &str;

  fn is_virtual(&self) -> bool;

  fn is_abstract(&self) -> bool;

  fn to_string(&self) -> String;

}

struct WrappedInstance<'a, 'b> {
  owner: godot::GDExtensionObjectPtr,
  interface: &'a GodotInterface<'b>,
  class: &'a dyn GodotExtensionClass,
}

pub(super) struct Userdata<'a, 'b> {
  pub(super) interface: &'a GodotInterface<'b>,
  pub(super) class: Box<dyn GodotExtensionClass>,
}

pub(super) extern fn set_func_bind(_: godot::GDExtensionClassInstancePtr, _: godot::GDExtensionConstStringNamePtr, _: godot::GDExtensionConstVariantPtr) -> u8 {
  println!("SET");
  0
}

pub(super) extern fn get_func_bind(_: godot::GDExtensionClassInstancePtr, _: godot::GDExtensionConstStringNamePtr, _: godot::GDExtensionVariantPtr) -> u8 {
  println!("BIND");
  0
}

pub(super) unsafe extern fn get_property_list_func_bind(_: godot::GDExtensionClassInstancePtr, count: *mut u32) -> *const godot::GDExtensionPropertyInfo {
  println!("CALL GET");
  *count = 0;
  ptr::null()
}

pub(super) extern fn free_property_list_func_bind(_: godot::GDExtensionClassInstancePtr, _: *const godot::GDExtensionPropertyInfo) {
  println!("FREEPROP");
}

pub(super) extern fn property_can_revert_func_bind(_: godot::GDExtensionClassInstancePtr, _: godot::GDExtensionConstStringNamePtr) -> u8 {
  println!("CANREV");
  0
}

pub(super) extern fn property_get_revert_func_bind(_: godot::GDExtensionClassInstancePtr, _: godot::GDExtensionConstStringNamePtr, _: godot::GDExtensionVariantPtr) -> u8 {
  println!("GETREV");
  0
}

pub(super) extern fn notification_func_bind(_: godot::GDExtensionClassInstancePtr, i: i32) {
  println!("NOTIF {}", i);
}

pub(super) unsafe extern fn to_string_func_bind(instance: godot::GDExtensionClassInstancePtr, is_valid: *mut godot::GDExtensionBool, string_ptr: godot::GDExtensionStringPtr) {
  let instance = instance as *mut WrappedInstance;
  let string = (*instance).class.to_string();
  let cstring = CString::new(string).unwrap();

  ((*instance).interface.string_new_with_utf8_chars)(string_ptr, cstring.as_ptr());
  *is_valid = 1;
}

pub(super) unsafe extern fn create_instance_func_bind(userdata: *mut c_void) -> godot::GDExtensionObjectPtr {
  let userdata = &*(userdata as *mut Userdata);

  let owner = {
    let parent_class_name = userdata.interface.string_name(userdata.class.parent_class_name());
    (userdata.interface.classdb_construct_object)(parent_class_name.opaque.as_ptr() as *const c_void)
  };

  let rust_data = Box::new(WrappedInstance { owner, class: &*userdata.class, interface: userdata.interface });
  let raw = Box::into_raw(rust_data);
  {
    let class_name = userdata.interface.string_name(userdata.class.class_name());
    (userdata.interface.object_set_instance)((*raw).owner, class_name.opaque.as_ptr() as *const c_void, raw as *mut c_void);
  }
  (*raw).owner
}

pub(super) extern fn get_virtual_func_bind(_: *mut c_void, _: godot::GDExtensionConstStringNamePtr) -> godot::GDExtensionClassCallVirtual {
  println!("GETVIRT");
  None
}

pub(super) unsafe extern fn free_instance_func_bind(_userdata: *mut c_void, instance: godot::GDExtensionClassInstancePtr) {
  let rust_data = Box::from_raw(instance as *mut WrappedInstance);
  drop(rust_data);
}
