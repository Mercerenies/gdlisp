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
  fn initialize_level(&mut self, interface: &mut GodotInterface<'_>, level: InitializationLevel);
  fn deinitialize_level(&mut self, interface: &mut GodotInterface<'_>, level: InitializationLevel);
}

struct InitObjectUserdata<'a, 'b> {
  implementation: GodotInterface<'a>,
  initializer: Box<dyn ExtensionInitializer + 'b>,
}

impl<'a> InitObject<'a> {

  pub fn new(interface: &'a GDExtensionInterface, library: GDExtensionClassLibraryPtr) -> Self {
    Self { interface, library }
  }

  pub fn setup_init<'b>(&self,
                        initializer: impl ExtensionInitializer + 'b,
                        godot_initialization: &mut godot::GDExtensionInitialization,
                        init_level: InitializationLevel) {
    // Note: In the current implementation, we do NOT free this
    // userdata at any point. This shouldn't be a problem since there
    // should only be one InitObject total for the duration of the
    // whole program, and InitObjectUserdata is (hopefully) a
    // relatively small structure.
    //
    // TODO Might be worth looking into whether or not we can safely
    // deallocate this in deinitialize(..).
    let userdata = Box::new(InitObjectUserdata {
      implementation: GodotInterface::new(self.interface, self.library),
      initializer: Box::new(initializer),
    });

    godot_initialization.userdata = Box::into_raw(userdata) as *mut c_void;
    godot_initialization.initialize = Some(Self::initialize);
    godot_initialization.deinitialize = Some(Self::deinitialize);
    godot_initialization.minimum_initialization_level = init_level.into_u32();
  }

  unsafe extern fn initialize(userdata: *mut c_void, level: godot::GDExtensionInitializationLevel) {
    let userdata = &mut *(userdata as *mut InitObjectUserdata);
    userdata.initializer.initialize_level(&mut userdata.implementation, InitializationLevel::from_u32(level));
  }

  unsafe extern fn deinitialize(userdata: *mut c_void, level: godot::GDExtensionInitializationLevel) {
    let userdata = &mut *(userdata as *mut InitObjectUserdata);
    userdata.initializer.deinitialize_level(&mut userdata.implementation, InitializationLevel::from_u32(level));
  }

}

impl InitObject<'static> {

  pub unsafe fn from_ptr(interface: *const GDExtensionInterface, library: GDExtensionClassLibraryPtr) -> Self {
    // Just a basic sanity check on the contents of the pointer.
    assert_eq!((*interface).version_major, 4, "GDExtensionInterface is not a Godot 4 object! Are you sure the pointer is valid?");

    Self::new(&*interface, library)
  }

}
