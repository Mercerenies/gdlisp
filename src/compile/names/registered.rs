
//! Provides the struct [`RegisteredNameGenerator`] for safely
//! generating (and registering) unused names based on a symbol table.

use super::generator::NameGenerator;
use super::contextual::ContextualNameGenerator;
use crate::ir::identifier::Namespace;
use crate::compile::symbol_table::SymbolTable;

/// A `RegisteredNameGenerator` is based on a [`SymbolTable`] and
/// generates names which do not appear in the symbol table.
///
/// A contextual name generator is designed to generate names in a
/// particular namespace and cannot be used in other namespaces, since
/// conflicts are only detected in one namespace. Whenever a name is
/// generated, it is also stored as a synthetic name in the symbol
/// table. For a variant of this type which does *not* modify the
/// symbol table, see [`super::contextual::ContextualNameGenerator`].
#[derive(Debug)]
pub struct RegisteredNameGenerator<'a> {
  context: &'a mut SymbolTable,
  namespace: Namespace,
  is_local: bool, // Note: Only used if namespace is Namespace::Value
}

impl<'a> RegisteredNameGenerator<'a> {

  /// Construct a new `RegisteredNameGenerator` for function names.
  pub fn new_fn(context: &mut SymbolTable) -> RegisteredNameGenerator<'_> {
    RegisteredNameGenerator {
      context: context,
      namespace: Namespace::Function,
      is_local: false,
    }
  }

  /// Construct a new `RegisteredNameGenerator` for *local* variable names.
  pub fn new_local_var(context: &mut SymbolTable) -> RegisteredNameGenerator<'_> {
    RegisteredNameGenerator {
      context: context,
      namespace: Namespace::Value,
      is_local: true,
    }
  }

  /// Construct a new `RegisteredNameGenerator` for *global* variable names.
  pub fn new_global_var(context: &mut SymbolTable) -> RegisteredNameGenerator<'_> {
    RegisteredNameGenerator {
      context: context,
      namespace: Namespace::Value,
      is_local: false,
    }
  }

  fn register_name(&mut self, name: String) {
    match self.namespace {
      Namespace::Value => self.context.add_synthetic_var(name, self.is_local),
      Namespace::Function => self.context.add_synthetic_fn(name),
    }
  }

}

impl<'a> NameGenerator for RegisteredNameGenerator<'a> {

  fn generate_with(&mut self, prefix: &str) -> String {
    // Get the name, using the same implementation as
    // ContextualNameGenerator.
    let mut contextual_generator = ContextualNameGenerator::new(self.context, self.namespace);
    let name = contextual_generator.generate_with(prefix);
    self.register_name(name.clone());
    name
  }

}
