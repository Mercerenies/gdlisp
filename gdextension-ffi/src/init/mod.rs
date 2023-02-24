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

pub mod level;

use crate::internal::godot::{self, GDExtensionInterface, GDExtensionClassLibraryPtr};
use crate::interface::GodotInterface;
use level::InitializationLevel;

use std::ffi::c_void;

#[derive(Debug)]
pub struct InitObject<'a> {
  interface: &'a GDExtensionInterface,
  library: GDExtensionClassLibraryPtr,
}

pub trait ExtensionInitializer {

  //pub fn initialize_level(&mut self, level: 

}

#[derive(Debug)]
struct InitObjectUserdata<'a> {
  implementation: GodotInterface<'a>,
}

impl<'a> InitObject<'a> {

  pub fn new(interface: &'a GDExtensionInterface, library: GDExtensionClassLibraryPtr) -> Self {
    Self { interface, library }
  }

  pub fn setup_init(&self, initialization: &mut godot::GDExtensionInitialization, init_level: InitializationLevel) {
    // Note: In the current implementation, we do NOT free this
    // userdata at any point. This shouldn't be a problem since there
    // should only be one InitObject total for the duration of the
    // whole program, and InitObjectUserdata is a relatively small
    // structure.
    //
    // TODO Might be worth looking into whether or not we can safely
    // deallocate this in deinitialize(..).
    let userdata = Box::new(InitObjectUserdata {
      implementation: GodotInterface::new(self.interface, self.library),
    });

    initialization.userdata = Box::into_raw(userdata) as *mut c_void;
    initialization.initialize = Some(Self::initialize);
    initialization.deinitialize = Some(Self::deinitialize);
    initialization.minimum_initialization_level = init_level.into_u32();
  }

  extern fn initialize(userdata: *mut c_void, level: godot::GDExtensionInitializationLevel) {

  }

  extern fn deinitialize(userdata: *mut c_void, level: godot::GDExtensionInitializationLevel) {

  }

}

impl InitObject<'static> {

  pub unsafe fn from_ptr(interface: *const GDExtensionInterface, library: GDExtensionClassLibraryPtr) -> Self {
    // Just a basic sanity check on the contents of the pointer.
    assert_eq!((*interface).version_major, 4, "GDExtensionInterface is not a Godot 4 object! Are you sure the pointer is valid?");

    Self::new(&*interface, library)
  }

}
