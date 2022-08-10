
//! Functions for identifying and detailing which names are in scope
//! in the IR.

pub mod decl;
pub mod name_table;
pub mod error;

use crate::ir::decl::{TopLevel, Decl, DeclF};
use crate::compile::error::GDError;
use crate::ir::identifier::Namespace;
use decl::{DeclScope, check_all_decl_scopes, ClassNamespaceAdaptor};
use name_table::NameTable;
use name_table::builder::NameTableBuilder;
use error::ScopeError;

struct ConcreteNamesAdaptor<'a>(&'a TopLevel);

/// Convenience method to perform all scope checking on an IR file
/// representation.
pub fn check_scopes(toplevel: &TopLevel) -> Result<(),  GDError> {
  check_all_decl_scopes(toplevel).map_err(GDError::from)?;
  check_main_class_conflicts(toplevel)
}

/// Check for conflicts between names defined in the main class and
/// names defined at the top level. There should be no overlap.
/// Returns an appropriate error in case of overlap.
///
/// If there is no main class, then this function simply returns
/// `Ok(())` without performing any further checks.
pub fn check_main_class_conflicts(toplevel: &TopLevel) -> Result<(), GDError> {
  let main_class_option = toplevel.find_main_class()?;
  if let Some(main_class) = main_class_option {

    // Get all toplevel names *except* `sys/declare` names.
    // `sys/declare` are specifically allowed to conflict with the
    // main class under this rule (this is necessary in the standard
    // library, where lots of supposedly module-level names are
    // `sys/declare` and are actually implemented on the main class
    // `GDLisp`).
    let concrete_names_adaptor = ConcreteNamesAdaptor(toplevel);
    let class_namespace_adaptor = ClassNamespaceAdaptor(&concrete_names_adaptor);
    let toplevel_names = class_namespace_adaptor.get_scope_names()?;
    let mut main_names = main_class.get_scope_names()?;
    main_names.retain(|ns, name, _| toplevel_names.has_name(ns, name));

    // If there are any conflicts, just report the first one.
    let first_conflict = main_names.iter().next();
    match first_conflict {
      None => Ok(()),
      Some((ns, name, pos)) => Err(GDError::from(ScopeError::NameConflictWithMainClass(ns, name.to_owned(), pos))),
    }
  } else {
    // No main class is declared, so conflict is impossible.
    Ok(())
  }
}

impl<'a> DeclScope<Namespace> for ConcreteNamesAdaptor<'a> {

  /// As `DeclScope` on [`TopLevel`] but excludes `sys/declare`
  /// declarations specifically. See the implementation of
  /// [`check_main_class_conflicts`] for why we care to do this.
  fn get_scope_names(&self) -> Result<NameTable<Namespace>, ScopeError<Namespace>> {
    let mut builder = NameTableBuilder::new();
    for decl in &self.0.decls {
      if !is_declare_decl(decl) {
        let namespace = decl.namespace();
        let name = decl.name().to_owned();
        builder.add_name(namespace, name, decl.pos)?;
      }
    }
    Ok(builder.build())
  }

}

fn is_declare_decl(decl: &Decl) -> bool {
  matches!(decl.value, DeclF::DeclareDecl(_))
}
