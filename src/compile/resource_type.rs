
//! The various types of resources GDLisp might encounter.
//!
//! [`ResourceType`] enumerates the possible resource types GDLisp
//! might attempt to load.

use crate::ir::import::{ImportDecl, ImportDetails};
use crate::pipeline::Pipeline;
use super::error::Error;

use std::convert::AsRef;
use std::path::Path;
use std::ffi::OsStr;

/// The various resource types a `use` statement might encounter.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResourceType {
  /// A GDLisp source file. Must be compiled to GDScript. GDLisp
  /// source files support the full breadth of import declaration
  /// types.
  GDLispSource,
  /// A GDScript source file. The file must exist, and the import must
  /// be a simple import or an aliased import. Open or restricted
  /// imports are not allowed.
  GDScriptSource,
  /// A packed scene file. The file must exist, and the import must be
  /// a simple import or an aliased import. Open or restricted imports
  /// are not allowed.
  PackedScene,
  /// Any other resource file. The file must exist, and the import
  /// must be a simple import or an aliased import. Open or restricted
  /// imports are not allowed.
  Miscellaneous,
}

impl ResourceType {

  /// Constructs a [`ResourceType`] for the type of resource at the
  /// given path. The file at `path` will not be read; all resource
  /// type inference is done by looking at the file name, specifically
  /// its extension.
  pub fn from_path<P : AsRef<Path> + ?Sized>(path: &P) -> ResourceType {
    ResourceType::from_file_extension(&path.as_ref().extension().unwrap_or_else(|| OsStr::new("")))
  }

  /// Returns the appropriate resource type given a file extension.
  /// `ext` should be an all-lowercase file extension excluding the
  /// initial dot. If the extension is not recognized, then
  /// [`ResourceType::Miscellaneous`] is returned.
  pub fn from_file_extension<S : AsRef<OsStr> + ?Sized>(ext: &S) -> ResourceType {
    let ext = ext.as_ref();
    if ext == "lisp" {
      ResourceType::GDLispSource
    } else if ext == "gd" {
      ResourceType::GDScriptSource
    } else if ext == "tscn" {
      ResourceType::PackedScene
    } else {
      ResourceType::Miscellaneous
    }
  }

  /// Whether or not the resource type can have macros defined in it.
  /// Only [`ResourceType::GDLispSource`] can have macros.
  pub fn can_have_macros(&self) -> bool {
    *self == ResourceType::GDLispSource
  }

  /// Whether the given import declaration is allowed for this
  /// particular resource type.
  ///
  /// GDLisp source files allow all import types, whereas other
  /// resource types are restricted to simple or aliased imports.
  pub fn is_import_allowed(&self, import: &ImportDecl) -> bool {
    if *self == ResourceType::GDLispSource {
      true // GDLispSource allows all imports
    } else {
      // For other resources, it must be ImportDetails::Named
      matches!(import.details, ImportDetails::Named(_))
    }
  }

  /// Checks [`ResourceType::is_import_allowed`]. If it is false, this
  /// method issues an appropriate error via `Err`. Otherwise, returns
  /// `Ok(())`.
  pub fn check_import(_pipeline: &Pipeline, import: &ImportDecl) -> Result<(), Error> {

    // if !pipeline.file_exists(import.filename.path()) {
    //   return Err(Error::ResourceDoesNotExist(import.filename.to_string()));
    // }

    let res_type = ResourceType::from_path(import.filename.path());
    if !res_type.is_import_allowed(import) {
      return Err(Error::InvalidImportOnResource(import.filename.to_string()));
    }

    Ok(())
  }

}

/// Construct a `ResourceType` from a reference to a path. Delegates
/// to [`ResourceType::from_path`].
impl From<&Path> for ResourceType {
  fn from(path: &Path) -> ResourceType {
    ResourceType::from_path(path)
  }
}

/// Construct a `ResourceType` from the path referenced by the import
/// declaration.
impl From<&ImportDecl> for ResourceType {
  fn from(imp: &ImportDecl) -> ResourceType {
    ResourceType::from_path(imp.filename.path())
  }
}
