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

//! Mechanism for controlling how GDScript `preload` calls are
//! resolved.
//!
//! Normally, when we need to transform a GDLisp `use` directive into
//! a GDScript `preload` call, we translate the name directly,
//! preserving the path and potentially changing the file extension
//! (`.lisp` to `.gd`). However, for macro loading, we may end up
//! needing to load using an alternative translation scheme, probably
//! to some temporary directory. The trait [`PreloadResolver`]
//! encompasses these behaviors. The first behavior is provided by the
//! singleton struct [`DefaultPreloadResolver`] and the second by
//! [`LookupPreloadResolver`].

use crate::runner::path::RPathBuf;
use crate::compile::resource_type::ResourceType;

use std::collections::HashMap;
use std::path::PathBuf;

/// A `DefaultPreloadResolver` is a [`PreloadResolver`] which passes
/// through all information. This is the default resolution rule, used
/// for generating runtime `.gd` files.
#[derive(Debug, Clone)]
pub struct DefaultPreloadResolver;

/// A [`LookupPreloadResolver`] acts as a sort of proxy, translating
/// names which reference real files into names which reference
/// temporary files in some virtual file system constructed by the
/// compiler.
///
/// A `LookupPreloadResolver` retains a hashmap mapping known
/// pathnames to virtual pathnames. This hashmap is used in
/// [`LookupPreloadResolver::resolve_preload`].
#[derive(Debug, Clone)]
pub struct LookupPreloadResolver(pub HashMap<PathBuf, PathBuf>);

/// A `PreloadResolver` controls how Godot `preload` calls will be
/// constructed and provides a way to inject custom pathname
/// resolution behavior into the compilation process.
pub trait PreloadResolver {

  /// Given a [`RPathBuf`], this method resolves the path into a
  /// Godot-friendly string, suitable for use as the argument to
  /// `preload`.
  ///
  /// If the `PreloadResolver` cannot resolve the path according to
  /// its rules, then `None` should be returned.
  fn resolve_preload(&self, path: &RPathBuf) -> Option<String>;

  /// Given a [`ResourceType`], indicates whether or not we should
  /// include the resource under `self`'s rules. If this returns
  /// false, then callers should replace the `preload` call with
  /// `null` or, if the resource is completely unavoidable, issue an
  /// error.
  fn include_resource(&self, res: ResourceType) -> bool;
}

impl LookupPreloadResolver {

  /// Add a new binding to the known paths for `self`, overwriting any
  /// existing bindings.
  ///
  /// If the key already existed in the `LookupPreloadResolver`,
  /// returns its previous value. Otherwise, returns `None`.
  pub fn insert(&mut self, key: PathBuf, value: PathBuf) -> Option<PathBuf> {
    self.0.insert(key, value)
  }

}

impl PreloadResolver for DefaultPreloadResolver {

  /// Calls `path.to_string` and succeeds unconditionally.
  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    Some(path.to_string())
  }

  /// Constantly returns true. All resource types are included in the
  /// default resolver rule.
  fn include_resource(&self, _res: ResourceType) -> bool {
    true
  }

}

impl PreloadResolver for LookupPreloadResolver {

  /// Translates the name according to the hashmap within `self`. If
  /// the name is not found in the hashmap, then this method returns
  /// `None`.
  fn resolve_preload(&self, path: &RPathBuf) -> Option<String> {
    self.0.get(path.path()).map(RPathBuf::path_to_string)
  }

  /// `LookupPreloadResolver` only suggests including GDLisp source
  /// files, i.e. [`ResourceType::GDLispSource`]. All other resource
  /// types result in false.
  fn include_resource(&self, res: ResourceType) -> bool {
    res == ResourceType::GDLispSource
  }

}
