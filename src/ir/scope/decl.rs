
//! Functions for determining scope at a high-level.
//!
//! These functions deal with high-level concepts such as classes and
//! declarations and are specifically *not* concerned with lower-level
//! concepts like `let` blocks and local variables.

use super::name_table::NameTable;
use super::name_table::builder::NameTableBuilder;
use super::error::ScopeError;
use crate::ir::identifier::{Namespace, ClassNamespace};
use crate::ir::decl::{TopLevel, ClassDecl};
use crate::gdscript::library;

use std::hash::Hash;

/// Trait for containers of declarations which can meaningfully
/// enumerate the names declared in their scope. Implementors of this
/// trait should only return a table of the names in the immediate
/// scope and should specifically *not* recurse on inner declarations,
/// such as the bodies of inner classes.
pub trait DeclScope<NS: Hash + Eq + Clone> {

  /// Returns a table of all names, or an appropriate [`ScopeError`]
  /// if a problem occurs during enumeration.
  fn get_scope_names(&self) -> Result<NameTable<NS>, ScopeError<NS>>;

}

impl DeclScope<Namespace> for TopLevel {

  /// Returns a table of all names, or an appropriate [`ScopeError`]
  /// if a problem occurs during enumeration.
  ///
  /// This method does *not* enumerate imported names. It only
  /// produces names for declarations which are actually defined in
  /// this current file, not those imported into scope.
  fn get_scope_names(&self) -> Result<NameTable<Namespace>, ScopeError<Namespace>> {
    let mut builder = NameTableBuilder::new();
    for decl in &self.decls {
      let namespace = decl.namespace();
      let name = decl.name().to_owned();
      builder.add_name(namespace, name, decl.pos)?;
    }
    Ok(builder.build())
  }

}

impl DeclScope<ClassNamespace> for ClassDecl {

  fn get_scope_names(&self) -> Result<NameTable<ClassNamespace>, ScopeError<ClassNamespace>> {
    let mut builder = NameTableBuilder::new();

    // Add the constructor as a special case
    if let Some(constructor) = &self.constructor {
      builder.add_name(ClassNamespace::Function, library::CONSTRUCTOR_NAME.to_owned(), constructor.body.pos)?;
    }

    for decl in &self.decls {
      let namespace = decl.namespace();
      let name = decl.name().to_owned();
      builder.add_name(namespace, name, decl.pos)?;
    }

    Ok(builder.build())
  }

}

// TODO Tests
