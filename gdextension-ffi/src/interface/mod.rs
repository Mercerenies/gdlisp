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
use data::GodotString;

use std::ffi::{c_void, c_char};

#[derive(Debug)]
pub struct GodotInterface<'a> {
  interface: &'a GDExtensionInterface,
  library: GDExtensionClassLibraryPtr,
  extension_api: ExtensionApi,
  string_new_with_utf8_chars: unsafe extern fn(*mut c_void, *const c_char),
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

    Self {
      interface,
      library,
      extension_api,
      string_new_with_utf8_chars,
    }
  }

  //pub fn string(&self, input_string: impl Into<Vec<u8>>) -> GodotString {
  //  (self.string_new_with_utf8_chars)
  //}

}
