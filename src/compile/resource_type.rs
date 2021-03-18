
use crate::ir::import::{ImportDecl, ImportDetails};
use crate::pipeline::Pipeline;
use super::error::Error;

use std::convert::AsRef;
use std::path::Path;
use std::ffi::OsStr;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResourceType {
  // A GDLisp source file. Must be compiled to GDScript. GDLisp source
  // files support the full breadth of import declaration types.
  GDLispSource,
  // A GDScript source file. The file must exist, and the import must
  // be a simple import or an aliased import. Open or restricted
  // imports are not allowed.
  GDScriptSource,
  // A packed scene file. The file must exist, and the import must be
  // a simple import or an aliased import. Open or restricted imports
  // are not allowed.
  PackedScene,
  // Any other resource file. The file must exist, and the import must
  // be a simple import or an aliased import. Open or restricted
  // imports are not allowed.
  Miscellaneous,
}

impl ResourceType {

  pub fn from_path<P : AsRef<Path> + ?Sized>(path: &P) -> ResourceType {
    ResourceType::from_file_extension(&path.as_ref().extension().unwrap_or(OsStr::new("")))
  }

  pub fn from_file_extension<S : AsRef<OsStr> + ?Sized>(ext: &S) -> ResourceType {
    let ext = ext.as_ref();
    if ext == "lisp" {
      ResourceType::GDLispSource
    } else if ext == "gd" {
      ResourceType::GDScriptSource
    } else if ext == "tres" {
      ResourceType::PackedScene
    } else {
      ResourceType::Miscellaneous
    }
  }

  pub fn can_have_macros(&self) -> bool {
    *self == ResourceType::GDLispSource
  }

  pub fn is_import_allowed(&self, import: &ImportDecl) -> bool {
    if *self == ResourceType::GDLispSource {
      true // GDLispSource allows all imports
    } else {
      // For other resources, it must be ImportDetails::Named
      matches!(import.details, ImportDetails::Named(_))
    }
  }

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
