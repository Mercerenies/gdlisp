
//! Export and visibility rules for GDLisp modules.

use super::decl::Decl;
use super::identifier::Id;

/// A name defined in a GDLisp module is either public or private. A
/// private name is only accessible from the current module and cannot
/// be imported into other modules. A public name can be imported and
/// used in other modules.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Visibility { Public, Private }

impl Visibility {

  /// Default visibility for function declarations.
  pub const FUNCTION: Visibility = Visibility::Public;

  /// Default visibility for macro declarations.
  pub const MACRO: Visibility = Visibility::Public;

  /// Default visibility for symbol macro declarations.
  pub const SYMBOL_MACRO: Visibility = Visibility::Public;

  /// Default visibility for constant declarations.
  pub const CONST: Visibility = Visibility::Public;

  /// Default visibility for class declarations.
  pub const CLASS: Visibility = Visibility::Public;

  /// Default visibility for object declarations.
  pub const OBJECT: Visibility = Visibility::Public;

  /// Default visibility for enum declarations.
  pub const ENUM: Visibility = Visibility::Public;

  /// Default visibility for `sys/declare` declarations.
  pub const DECLARE: Visibility = Visibility::Private;

}

/// Returns a vector of identifiers exported from the declarations
/// `decls`. Private identifiers are *not* included in this vector.
pub fn get_export_list<'a>(decls: impl IntoIterator<Item=&'a Decl>) -> Vec<Id> {
  let mut exports = Vec::new();
  for decl in decls {
    if decl.visibility() == Visibility::Public {
      exports.push(decl.to_id());
    }
  }
  exports
}
