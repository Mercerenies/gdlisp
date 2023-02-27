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

use gdextension_api::{ExtensionApi, load_extension_api_from_str};
use crate::internal::godot::{GDExtensionInterface, GDExtensionClassLibraryPtr};
use crate::get_build_config;
use data::GodotString;

use std::ffi::{c_void, c_char};

#[derive(Debug)]
pub struct GodotInterface<'a> {
  interface: &'a GDExtensionInterface,
  library: GDExtensionClassLibraryPtr,
  extension_api: ExtensionApi,
  sizes: Sizes,
  string_new_with_utf8_chars: unsafe extern fn(*mut c_void, *const c_char),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Sizes {
  pub string: usize,
}

static JSON_EXTENSION_DATA: &'static str = include_str!("../../extension_api.json");

impl<'a> GodotInterface<'a> {

  pub fn new(interface: &'a GDExtensionInterface, library: GDExtensionClassLibraryPtr) -> Self {
    // Go ahead and extract pointers to all of the library functions
    // we need. If any are null for some reason, we catch it now
    // rather than later.
    let string_new_with_utf8_chars = interface.string_new_with_utf8_chars.unwrap();

    // Load the extension API from the JSON file.
    let extension_api = load_extension_api_from_str(JSON_EXTENSION_DATA).unwrap();
    let sizes = Sizes::new(&extension_api);

    Self {
      interface,
      library,
      extension_api,
      sizes,
      string_new_with_utf8_chars,
    }
  }

  pub fn string(&self, input_string: impl Into<Vec<u8>>) -> GodotString {
    let mut s = GodotString { opaque: alloc(self.sizes.string) };
    let cstring = CString::new(input_string);
    unsafe {
      (self.string_new_with_utf8_chars)(
  }

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
    }
  }

}
