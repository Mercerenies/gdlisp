
use super::decl::Decl;

// For now, this is pretty simple; any name defined in the current
// file (i.e. not imported or built-in) is exported. We'll provide
// tons of capabilities to customize this later.

pub fn get_export_list<'a>(decls: impl IntoIterator<Item=&'a Decl>) -> Vec<String> {
  let mut exports = Vec::new();
  for decl in decls {
    exports.push(decl.name().to_owned());
  }
  exports
}
