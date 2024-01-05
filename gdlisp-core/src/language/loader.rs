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

use crate::script::GDLispScript;
use crate::singleton::GodotSingleton;
use gdlisp_util::path::RPathBuf;

use godot::prelude::*;
use godot::engine::{ResourceLoader, ResourceFormatLoader, IResourceFormatLoader, FileAccess};
use godot::engine::file_access::ModeFlags;
use godot::engine::notify::ObjectNotification;

use std::convert::TryFrom;

#[derive(Debug, GodotClass)]
#[class(base=ResourceFormatLoader)]
pub struct GDLispResourceFormatLoader {
  #[base]
  base: Base<ResourceFormatLoader>,
}

#[godot_api]
impl GDLispResourceFormatLoader {}

impl GodotSingleton for GDLispResourceFormatLoader {
  const CLASS_NAME: &'static str = "GDLispResourceFormatLoader";

  fn allocate_singleton() -> Gd<Self> {
    let instance = Self::new_gd();
    ResourceLoader::singleton().add_resource_format_loader(instance.clone().upcast());
    instance
  }

  fn deallocate_singleton(instance: Gd<Self>) {
    ResourceLoader::singleton().remove_resource_format_loader(instance.upcast());
    // No need to explicitly free, as GDLispResourceFormatLoader is
    // RefCounted.
  }
}

#[godot_api]
impl IResourceFormatLoader for GDLispResourceFormatLoader {

  fn init(base: Base<ResourceFormatLoader>) -> Self {
    GDLispResourceFormatLoader { base }
  }

  fn to_string(&self) -> GString {
    GString::from(Self::CLASS_NAME)
  }

  fn on_notification(&mut self, _what: ObjectNotification) {
    // Empty method.
  }

  fn get_recognized_extensions(&self) -> PackedStringArray {
    PackedStringArray::from(&[GString::from("lisp")])
  }

  fn handles_type(&self, type_: StringName) -> bool {
    println!("GDLispResourceFormatLoader.handles_type {}", &type_);
    type_ == StringName::from("Script") || type_ == StringName::from("GDLispScript")
  }

  fn get_resource_type(&self, path: GString) -> GString {
    if let Ok(path) = RPathBuf::try_from(path.to_string()) {
      if path.extension().map_or(false, |x| x.to_ascii_lowercase() == "lisp") {
        return GString::from("GDLispScript");
      }
    }
    GString::new()
  }

  fn get_resource_script_class(&self, _path: GString) -> GString {
    // TODO (Named classes)
    println!("GDLispResourceFormatLoader.get_resource_script_class");
    GString::new()
  }

  fn load(
    &self,
    path: GString,
    original_path: GString,
    _use_sub_threads: bool,
    _cache_mode: i32,
  ) -> Variant {
    // TODO Caching
    println!("GDLispResourceFormatLoader.load {}", &path);
    match FileAccess::open(path, ModeFlags::READ) {
      None => {
        Variant::from(FileAccess::get_open_error())
      }
      Some(file) => {
        let mut resource: Gd<GDLispScript> = GDLispScript::new_gd();
        resource.set_source_code(file.get_as_text());
        resource.set_path(original_path);
        Variant::from(resource)
      }
    }
  }

}
