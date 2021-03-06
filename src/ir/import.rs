
use crate::sxp::ast::AST;
use crate::sxp::dotted::DottedExpr;
use crate::runner::path::{RPathBuf, PathSrc};
use super::identifier::{Namespace, Id, IdLike};

use std::convert::{TryInto, TryFrom};
use std::fmt;

// Import syntax:
//
// (1) Qualified import
// (use "res://example/foo.lisp")
// Imports "example/foo.lisp" as example/foo
//
// (2) Qualified import (aliased)
// (use "res://example/foo.lisp" as renamed-foo)
// Imports "example/foo.lisp" as renamed-foo
//
// (3) Explicit import
// (use "res://example/foo.lisp" (a b c (d function)))
// Imports functions a, b, c from "example/foo.lisp"
//
// (4) Explicit import (aliased)
// (use "res://example/foo.lisp" ((a as a1) (b as b1) (c as c1) (d value as d1)))
// Imports functions a, b, c as a1, b1, c1 from "example/foo.lisp"
//
// (5) Wildcard import
// (use "res://example/foo.lisp" open)
// Imports all names into the current scope

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportDecl {
  pub filename: RPathBuf,
  pub details: ImportDetails,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportDetails {
  Named(String),                                  // (1) and (2) above
  Restricted(Vec<ImportName<Option<Namespace>>>), // (3) and (4) above
  Open,                                           // (5) above
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportName<NS> {
  pub namespace: NS,
  pub in_name: String,
  pub out_name: String,
}

#[derive(Debug)]
pub enum ImportDeclParseError {
  NoFilename,
  BadFilename(AST),
  InvalidPath(String),
  MalformedFunctionImport(AST),
  InvalidEnding(AST),
}

#[derive(Debug)]
pub enum ImportNameResolutionError {
  UnknownName(Id),
  AmbiguousNamespace(String),
}

impl ImportDecl {

  pub fn default_import_name(path: &RPathBuf) -> String {
    let mut path = path.clone();
    path.path_mut().set_extension("");
    path
      .components_no_root()
      .filter_map(|x| x.as_os_str().to_str())
      .collect::<Vec<_>>()
      .join("/")
  }

  pub fn parse_path_param(arg: &str) -> Option<RPathBuf> {
    // Paths must start with "res://"
    RPathBuf::try_from(String::from(arg)).ok().filter(|path| {
      path.source() == PathSrc::Res
    })
  }

  pub fn named(filename: RPathBuf, name: Option<String>) -> ImportDecl {
    let name = name.unwrap_or_else(|| ImportDecl::default_import_name(&filename));
    ImportDecl {
      filename: filename,
      details: ImportDetails::Named(name),
    }
  }

  pub fn restricted(filename: RPathBuf, imports: Vec<ImportName<Option<Namespace>>>) -> ImportDecl {
    ImportDecl {
      filename: filename,
      details: ImportDetails::Restricted(imports),
    }
  }

  pub fn open(filename: RPathBuf) -> ImportDecl {
    ImportDecl {
      filename: filename,
      details: ImportDetails::Open,
    }
  }

  pub fn names(&self, exports: &[Id]) -> Vec<ImportName<Namespace>> {
    exports.iter().cloned().filter_map(|export| {
      let Id { namespace: export_namespace, name: export_name } = export;
      let import_name = match &self.details {
        ImportDetails::Named(s) => {
          Some(format!("{}/{}", s, export_name))
        }
        ImportDetails::Open => {
          Some(export_name.clone())
        }
        ImportDetails::Restricted(vec) => {
          // Find it in the import list.
          if let Some(name_match) = vec.iter().find(|x| x.namespace.map_or(true, |ns| ns == export_namespace) && x.out_name == *export_name) {
            Some(name_match.in_name.clone())
          } else {
            None
          }
        }
      };
      import_name.map(|import_name| {
        ImportName::new(export_namespace, import_name, export_name)
      })
    }).collect()
  }

  pub fn parse(tail: &[&AST]) -> Result<ImportDecl, ImportDeclParseError> {
    if tail.is_empty() {
      return Err(ImportDeclParseError::NoFilename);
    }
    let filename = match tail[0] {
      AST::String(s) => ImportDecl::parse_path_param(s).ok_or_else(|| {
        ImportDeclParseError::InvalidPath(s.clone())
      }),
      x => Err(ImportDeclParseError::BadFilename(x.clone())),
    }?;
    match tail.len() {
      0 => { unreachable!() } // We checked tail.is_empty() already
      1 => {
        // (1) Qualified import
        Ok(ImportDecl::named(filename, None))
      }
      2 => {
        match tail[1] {
          AST::Symbol(open) if open == "open" => {
            // (5) Wildcard import
            Ok(ImportDecl::open(filename))
          }
          AST::Nil | AST::Cons(_, _) => {
            // (3) or (4) Explicit import (possibly aliased)
            let imports: Vec<_> = DottedExpr::new(tail[1]).try_into().map_err(|_| invalid_ending_err(&tail[1..]))?;
            let imports = imports.into_iter()
              .map(|x| ImportName::<Option<Namespace>>::parse(x))
              .collect::<Result<Vec<_>, _>>()?;
            Ok(ImportDecl::restricted(filename, imports))
          }
          _ => {
            Err(invalid_ending_err(&tail[1..]))
          }
        }
      }
      3 => {
        // (2) Qualified import (aliased)
        if *tail[1] != AST::symbol("as") {
          return Err(invalid_ending_err(&tail[1..]));
        }
        match tail[2] {
          AST::Symbol(s) => Ok(ImportDecl::named(filename, Some(s.clone()))),
          _ => Err(invalid_ending_err(&tail[1..]))
        }
      }
      _ => {
        Err(invalid_ending_err(&tail[1..]))
      }
    }
  }

}

impl<NS> ImportName<NS> {

  pub fn new(namespace: NS, in_name: String, out_name: String) -> ImportName<NS> {
    ImportName { namespace, in_name, out_name }
  }

  pub fn simple(namespace: NS, in_name: String) -> ImportName<NS> {
    let out_name = in_name.clone();
    ImportName { namespace, in_name, out_name }
  }

  fn symbol_to_namespace(symbol: &str) -> Result<Namespace, ImportDeclParseError> {
    match symbol {
      "value" => Ok(Namespace::Value),
      "function" => Ok(Namespace::Function),
      _ => Err(ImportDeclParseError::MalformedFunctionImport(AST::Symbol(symbol.to_owned()))),
    }
  }

  pub fn parse(clause: &AST) -> Result<ImportName<Option<Namespace>>, ImportDeclParseError> {
    match clause {
      AST::Symbol(s) => {
        Ok(ImportName::simple(None, s.clone()))
      }
      AST::Cons(_, _) => {
        let vec: Vec<_> = DottedExpr::new(clause).try_into().map_err(|_| ImportDeclParseError::MalformedFunctionImport(clause.clone()))?;
        match vec.as_slice() {
          [AST::Symbol(o), AST::Symbol(as_), AST::Symbol(i)] if as_ == "as" => {
            Ok(ImportName::new(None, i.clone(), o.clone()))
          }
          [AST::Symbol(o), AST::Symbol(ns), AST::Symbol(as_), AST::Symbol(i)] if as_ == "as" => {
            let ns = ImportName::<Option<Namespace>>::symbol_to_namespace(ns)?;
            Ok(ImportName::new(Some(ns), i.clone(), o.clone()))
          }
          [AST::Symbol(o), AST::Symbol(ns)] => {
            let ns = ImportName::<Option<Namespace>>::symbol_to_namespace(ns)?;
            Ok(ImportName::new(Some(ns), o.clone(), o.clone()))
          }
          _ => {
            Err(ImportDeclParseError::MalformedFunctionImport(clause.clone()))
          }
        }
      }
      _ => {
        Err(ImportDeclParseError::MalformedFunctionImport(clause.clone()))
      }
    }
  }

}

impl ImportName<Option<Namespace>> {

  pub fn refine(&self, exports: &[Id]) -> Result<ImportName<Namespace>, ImportNameResolutionError> {
    let mut matches = Vec::new();
    for export_id in exports {
      if self.namespace.map_or(true, |ns| ns == export_id.namespace) && export_id.name == self.out_name {
        matches.push(export_id);
      }
    }
    if matches.is_empty() {
      Err(ImportNameResolutionError::UnknownName(Id::new(self.namespace.unwrap_or(Namespace::Function), self.out_name.to_owned())))
    } else if matches.len() == 1 {
      Ok(ImportName::new(matches[0].namespace, self.in_name.to_owned(), self.out_name.to_owned()))
    } else {
      Err(ImportNameResolutionError::AmbiguousNamespace(self.out_name.to_owned()))
    }
  }

}

impl ImportName<Namespace> {

  pub fn into_imported_id(self) -> Id {
    Id::new(self.namespace, self.in_name)
  }

  pub fn into_exported_id(self) -> Id {
    Id::new(self.namespace, self.out_name)
  }

  pub fn to_imported_id<'a>(&'a self) -> Box<dyn IdLike + 'a> {
    Id::build(self.namespace, &self.in_name)
  }

  pub fn to_exported_id<'a>(&'a self) -> Box<dyn IdLike + 'a> {
    Id::build(self.namespace, &self.out_name)
  }

}

fn invalid_ending_err(tail: &[&AST]) -> ImportDeclParseError {
  let ending: Vec<AST> = tail.iter().map(|x| (*x).clone()).collect();
  ImportDeclParseError::InvalidEnding(AST::list(ending))
}

impl fmt::Display for ImportDeclParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ImportDeclParseError::NoFilename => {
        write!(f, "Expected filename in import")
      }
      ImportDeclParseError::BadFilename(ast) => {
        write!(f, "Not a valid filename in import {}", ast)
      }
      ImportDeclParseError::InvalidPath(s) => {
        write!(f, "Not a valid path in import {}", s)
      }
      ImportDeclParseError::MalformedFunctionImport(ast) => {
        write!(f, "Malformed function import {}", ast)
      }
      ImportDeclParseError::InvalidEnding(ast) => {
        write!(f, "Invalid end of import {}", ast)
      }
    }
  }
}

impl fmt::Display for ImportNameResolutionError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ImportNameResolutionError::UnknownName(id) => {
        write!(f, "Unknown name in import resolution {}", id.name)
      }
      ImportNameResolutionError::AmbiguousNamespace(name) => {
        write!(f, "Ambiguous namespace at {} in import", name)
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::parser;

  fn parse_ast(input: &str) -> AST {
    let parser = parser::ASTParser::new();
    parser.parse(input).unwrap()
  }

  fn parse_import(input: &str) -> Result<ImportDecl, ImportDeclParseError> {
    let ast = parse_ast(input);
    let dotted: Vec<_> = DottedExpr::new(&ast).try_into().unwrap();
    ImportDecl::parse(&dotted)
  }

  fn str_to_rpathbuf(input: &str) -> RPathBuf {
    RPathBuf::try_from(String::from(input)).unwrap()
  }

  #[test]
  fn default_import_name_test() {
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("/a/b/c")), "a/b/c");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("/abcd")), "abcd");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://foo/bar")), "foo/bar");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://foo/bar.lisp")), "foo/bar");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://foo/bar.gd")), "foo/bar");
  }

  #[test]
  fn test_parsing() {
    assert_eq!(parse_import(r#"("res://foo/bar")"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), None));
    assert_eq!(parse_import(r#"("res://foo/bar")"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), Some(String::from("foo/bar"))));
    assert_eq!(parse_import(r#"("res://foo/bar" as foo)"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), Some(String::from("foo"))));
    assert_eq!(parse_import(r#"("res://foo/bar" as foo.baz)"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), Some(String::from("foo.baz"))));
    assert_eq!(parse_import(r#"("res://foo/bar" open)"#).unwrap(),
               ImportDecl::open(str_to_rpathbuf("res://foo/bar")));
    assert_eq!(parse_import(r#"("res://foo/bar" (a b))"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(ImportName::simple(None, String::from("a")),
                                           ImportName::simple(None, String::from("b")))));
    assert_eq!(parse_import(r#"("res://foo/bar" ())"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!()));
    assert_eq!(parse_import(r#"("res://foo/bar" ((a as a1) b))"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(ImportName::new(None, String::from("a1"), String::from("a")),
                                           ImportName::simple(None, String::from("b")))));
    assert_eq!(parse_import(r#"("res://foo/bar" ((a function as a1) (b value)))"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(ImportName::new(Some(Namespace::Function), String::from("a1"), String::from("a")),
                                           ImportName::simple(Some(Namespace::Value), String::from("b")))));
  }

  #[test]
  fn test_invalid_parsing() {
    assert!(parse_import(r#"(10)"#).is_err());
    assert!(parse_import(r#"("foo/bar")"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" as a as b)"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" as 1)"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" open-NOT-CORRECT)"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" (1))"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" ((a as)))"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" ((a b c)))"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" ([]))"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" (()))"#).is_err());
    assert!(parse_import(r#"("res://foo/bar" [])"#).is_err());
  }

}
