
use std::path::{PathBuf, Path};

// Import syntax:
//
// (1) Qualified import
// (use "res://example/foo.lisp")
// Imports "example/foo.lisp" as example.foo
//
// (2) Qualified import (aliased)
// (use "res://example/foo.lisp" as renamed-foo)
// Imports "example/foo.lisp" as renamed-foo
//
// (3) Explicit import
// (use "res://example/foo.lisp" (a b c))
// Imports functions a, b, c from "example/foo.lisp"
//
// (3) Explicit import (aliased)
// (use "res://example/foo.lisp" ((a as a1) (b as b1) (c as c1)))
// Imports functions a, b, c as a1, b1, c1 from "example/foo.lisp"
//
// (4) Wildcard import
// (use "res://example/foo.lisp" open)
// Imports all names into the current scope

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportDecl {
  pub filename: PathBuf,
  pub details: ImportDetails,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportDetails {
  Renamed(String),             // (1) and (2) above
  Restricted(Vec<ImportName>), // (3) and (4) above
  Open,                        // (5) above
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportName {
  pub in_name: String,
  pub out_name: String,
}

impl ImportDecl {

  pub fn default_import_name<P : AsRef<Path> + ?Sized>(path: &P) -> String {
    let path = path.as_ref();
    let mut comp: Vec<_> = path.components().collect();
    if comp.get(0).and_then(|x| x.clone().as_os_str().to_str()) == Some("res:") {
      comp.remove(0);
    }
    comp.into_iter().filter_map(|x| x.as_os_str().to_str()).collect::<Vec<_>>().join(".")
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn default_import_name_test() {
    assert_eq!(ImportDecl::default_import_name("a/b/c"), "a.b.c");
    assert_eq!(ImportDecl::default_import_name("abcd"), "abcd");
    assert_eq!(ImportDecl::default_import_name("res://foo/bar"), "foo.bar");
  }

}
