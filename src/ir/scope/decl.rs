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

//! Functions for determining scope at a high-level.
//!
//! These functions deal with high-level concepts such as classes and
//! declarations and are specifically *not* concerned with lower-level
//! concepts like `let` blocks and local variables.

use super::name_table::NameTable;
use super::name_table::builder::NameTableBuilder;
use super::error::ScopeError;
use crate::util::extract_err;
use crate::pipeline::source::SourceOffset;
use crate::ir::identifier::{Namespace, ClassNamespace};
use crate::ir::decl::{Decl, DeclF, TopLevel, ClassDecl};
use crate::ir::expr::{Expr, ExprF, LambdaClass};
use crate::ir::literal::Literal;
use crate::gdscript::library;
use crate::optimize::ir::expr_walker::walk_exprs_in_decl;

use std::hash::Hash;
use std::convert::Infallible;

/// Any type which implements [`DeclScope`] for the namespace
/// [`Namespace`] can also correctly implement it for
/// [`ClassNamespace`] via a simple embedding (`From::from`). The
/// `ClassNamespaceAdaptor` type takes a value which implements
/// `DeclScope<Namespace>` and provides a value which implements
/// `DeclScope<ClassNamespace>`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassNamespaceAdaptor<'a, T>(pub &'a T);

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

impl<'a, T> DeclScope<ClassNamespace> for ClassNamespaceAdaptor<'a, T>
where T: DeclScope<Namespace> {

  fn get_scope_names(&self) -> Result<NameTable<ClassNamespace>, ScopeError<ClassNamespace>> {
    self.0.get_scope_names()
      .map_err(ScopeError::from)
      .map(|table| table.map_ns(ClassNamespace::from))
  }

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
      let name = decl.name().into_owned();
      builder.add_name(namespace, name, decl.pos)?;
    }

    Ok(builder.build())
  }

}

impl DeclScope<ClassNamespace> for LambdaClass {

  fn get_scope_names(&self) -> Result<NameTable<ClassNamespace>, ScopeError<ClassNamespace>> {
    let mut builder = NameTableBuilder::new();

    // Add the constructor as a special case
    if let Some(constructor) = &self.constructor {
      builder.add_name(ClassNamespace::Function, library::CONSTRUCTOR_NAME.to_owned(), constructor.body.pos)?;
    }

    for decl in &self.decls {
      let namespace = decl.namespace();
      let name = decl.name().into_owned();
      builder.add_name(namespace, name, decl.pos)?;
    }

    Ok(builder.build())
  }

}

impl<'a, T, NS: Hash + Eq + Clone> DeclScope<NS> for &'a T
where T: DeclScope<NS> {
  fn get_scope_names(&self) -> Result<NameTable<NS>, ScopeError<NS>> {
    (*self).get_scope_names()
  }
}

/// Return a vector of all of the declaration scopes in the given
/// file, in an unspecified order.
///
/// This function returns all of the following.
/// * The toplevel scope itself.
/// * Any class scopes introduced in the file, including the main class, which
///   is considered distinct from the toplevel.
/// * Any anonymous classes defined in the file.
#[allow(clippy::vec_init_then_push)] // For style consistency
pub fn get_all_decl_scopes<'a>(toplevel: &'a TopLevel) -> Vec<Box<dyn DeclScope<ClassNamespace> + 'a>> {
  let mut acc: Vec<Box<dyn DeclScope<ClassNamespace> + 'a>> = Vec::new();

  // The toplevel scope
  acc.push(Box::new(ClassNamespaceAdaptor(toplevel)));

  // Any classes defined in the toplevel
  for decl in &toplevel.decls {
    match &decl.value {
      DeclF::ClassDecl(cls) => {
        acc.push(Box::new(cls));
      }
      DeclF::FnDecl(_) | DeclF::MacroDecl(_) | DeclF::SymbolMacroDecl(_) | DeclF::ConstDecl(_) | DeclF::EnumDecl(_) | DeclF::DeclareDecl(_) => {}
    }
  }

  // Any anonymous classes declared in the file
  let err = on_each_lambda_class::<Infallible, _>(&toplevel.decls, |class| {
    // *sigh* What an unfortunate copy. But I don't see a way to
    // convince the borrow checker that this value isn't going to
    // disappear. (TODO Yeah...)
    acc.push(Box::new(class.clone()));
    Ok(())
  });
  extract_err(err);

  acc
}

/// Given a file, run the check for duplicate names, returning an
/// error if any are found. If no errors occur, returns normally.
pub fn check_all_decl_scopes(toplevel: &TopLevel) -> Result<(), ScopeError<ClassNamespace>> {
  for scope in get_all_decl_scopes(toplevel) {
    // We don't need the resulting table; just any errors that occur
    // while trying to produce it.
    let _ = scope.get_scope_names()?;
  }
  Ok(())
}

pub fn on_each_lambda_class<E, F>(decls: &[Decl], mut block: F) -> Result<(), E>
where F : FnMut(&LambdaClass) -> Result<(), E> {
  for decl in decls {
    walk_exprs_in_decl(decl, |expr| {
      if let ExprF::LambdaClass(cls) = &expr.value {
        block(cls)?;
      }
      // Note: We don't use this value, so if this string ever appears
      // in the output code, there's a problem.
      Ok(Expr::literal(Literal::from("UNUSED STRING FROM on_each_lambda_class"), SourceOffset(0)))
    })?;
  }
  Ok(())
}

// TODO Tests
