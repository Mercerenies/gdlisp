
use super::decl::Decl;
use super::identifier::Id;

// For now, this is pretty simple; any name defined in the current
// file (i.e. not imported or built-in) is exported. We'll provide
// tons of capabilities to customize this later.

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Visibility { Public, Private }

impl Visibility {
  // Default visibilities for the various declaration types.
  pub const FUNCTION: Visibility = Visibility::Public;
  pub const MACRO: Visibility = Visibility::Public;
  pub const CONST: Visibility = Visibility::Public;
  pub const ENUM: Visibility = Visibility::Public;
  pub const DECLARE: Visibility = Visibility::Private;
}

pub fn get_export_list<'a>(decls: impl IntoIterator<Item=&'a Decl>) -> Vec<Id> {
  let mut exports = Vec::new();
  for decl in decls {
    if decl.visibility() == Visibility::Public {
      exports.push(decl.to_id());
    }
  }
  exports
}
