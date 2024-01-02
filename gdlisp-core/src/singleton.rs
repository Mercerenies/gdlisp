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

use godot::prelude::*;
use godot::engine::Engine;
use godot::obj::UserClass;
use godot::obj::cap::GodotDefault;

/// Trait for Rust-side objects that will exist as a named singleton
/// on `Engine` in Godot.
pub trait GodotSingleton: UserClass + GodotDefault + Inherits<Object> {

  /// The name of the singleton on the Godot side. This will usually
  /// just be the name of the implementing Rust struct.
  const CLASS_NAME: &'static str;

  /// Allocates an instance to be used as the singleton object. This
  /// should usually be a call to either Self::new_gd (for
  /// `RefCounted`) or Self::alloc_gd (for other types).
  fn allocate_singleton() -> Gd<Self>;

  /// Frees the instance referenced by the argument. For `RefCounted`
  /// types, it's reasonable to implement this method with an empty
  /// body.
  fn deallocate_singleton(instance: Gd<Self>);

  /// Returns the singleton from `Engine`, or `None` if none exists.
  fn get_singleton() -> Option<Gd<Self>> {
    // Note: If Engine::get_singleton returns None, Godot prints a
    // warning to the console. So we have to pre-emptively check
    // whether the singleton exists *before* getting it to avoid the
    // warning.
    let class_name = StringName::from(Self::CLASS_NAME);
    let engine = Engine::singleton();
    if engine.has_singleton(class_name.clone()) {
      let singleton = Engine::singleton().get_singleton(class_name).expect("Singleton in inconsistent state");
      singleton.try_cast().ok()
    } else {
      None
    }
  }

  /// Initializes the singleton and returns it. If the singleton has
  /// already been initialized, returns the existing instance without
  /// creating a new one.
  fn initialize_singleton() -> Gd<Self> {
    if let Some(existing_singleton) = Self::get_singleton() {
      return existing_singleton;
    }

    let instance = Self::allocate_singleton();
    Engine::singleton().register_singleton(StringName::from(Self::CLASS_NAME), instance.clone().upcast());
    instance
  }

  /// Releases and frees the singleton, unbinding it from `Engine` in
  /// the process. Any references to the singleton should be
  /// considered invalidated after this method is called. If the
  /// singleton has not been initialized, this method is a no-op.
  fn deinitialize_singleton() {
    if let Some(instance) = Self::get_singleton() {
      Self::deallocate_singleton(instance);
      Engine::singleton().unregister_singleton(StringName::from(Self::CLASS_NAME));
    }
  }

}
