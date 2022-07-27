
//! Provides the struct [`ContextualNameGenerator`] for safely
//! generating unused names based on a symbol table.

use super::generator::NameGenerator;
use crate::ir::identifier::Namespace;
use crate::compile::symbol_table::SymbolTable;

use std::borrow::ToOwned;

/// A `ContextualNameGenerator` is based on a [`SymbolTable`] and
/// generates names which do not appear in the symbol table.
///
/// A contextual name generator is designed to generate names in a
/// particular namespace and cannot be used in other namespaces, since
/// conflicts are only detected in one namespace.
#[derive(Debug, Clone)]
pub struct ContextualNameGenerator<'a> {
  context: &'a SymbolTable,
  namespace: Namespace,
}

impl<'a> ContextualNameGenerator<'a> {

  /// Construct a new `ContextualNameGenerator` from a symbol table.
  pub fn new(namespace: Namespace, context: &SymbolTable) -> ContextualNameGenerator<'_> {
    ContextualNameGenerator { context, namespace }
  }

  fn is_name_in_use(&self, name: &str) -> bool {
    match self.namespace {
      Namespace::Value => self.context.has_var_with_gd_name(name),
      Namespace::Function => self.context.has_fn_with_gd_name(name),
    }
  }

}

impl<'a> NameGenerator for ContextualNameGenerator<'a> {

  fn generate_with(&mut self, prefix: &str) -> String {
    let mut index = -1;
    let mut name: String = prefix.to_owned();
    while self.is_name_in_use(&name) {
      index += 1;
      name = format!("{}_{}", prefix, index);
    }
    name
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn empty_generate() {
    let table = SymbolTable::new();
    let mut gen = ContextualNameGenerator::new(Namespace::Value, &table);
    assert_eq!(gen.generate(), "_G");
    assert_eq!(gen.generate(), "_G");
    assert_eq!(gen.generate(), "_G");
  }

  #[test]
  fn contextual_generate() {
    let mut table = SymbolTable::new();
    table.add_synthetic_var(String::from("_G"));
    let mut gen = ContextualNameGenerator::new(Namespace::Value, &table);
    assert_eq!(gen.generate(), "_G_0");
    assert_eq!(gen.generate(), "_G_0");
    assert_eq!(gen.generate(), "_G_0");
  }

}
