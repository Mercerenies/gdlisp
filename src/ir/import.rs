
use crate::sxp::ast::{AST, ASTF};
use crate::sxp::literal::Literal;
use crate::sxp::dotted::DottedExpr;
use crate::runner::path::{RPathBuf, PathSrc};
use crate::pipeline::source::SourceOffset;
use super::identifier::{Namespace, Id, IdLike};

use std::convert::{TryInto, TryFrom};
use std::fmt;
use std::error::Error;

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
  pub pos: SourceOffset,
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

#[derive(Debug, PartialEq, Eq)]
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
      .last()
      .unwrap_or("/")
      .to_owned()
  }

  pub fn parse_path_param(arg: &str) -> Option<RPathBuf> {
    // Paths must start with "res://"
    RPathBuf::try_from(String::from(arg)).ok().filter(|path| {
      path.source() == PathSrc::Res
    })
  }

  pub fn named(filename: RPathBuf, name: Option<String>, pos: SourceOffset) -> ImportDecl {
    let name = name.unwrap_or_else(|| ImportDecl::default_import_name(&filename));
    ImportDecl {
      filename: filename,
      details: ImportDetails::Named(name),
      pos,
    }
  }

  pub fn restricted(filename: RPathBuf, imports: Vec<ImportName<Option<Namespace>>>, pos: SourceOffset) -> ImportDecl {
    ImportDecl {
      filename: filename,
      details: ImportDetails::Restricted(imports),
      pos,
    }
  }

  pub fn open(filename: RPathBuf, pos: SourceOffset) -> ImportDecl {
    ImportDecl {
      filename: filename,
      details: ImportDetails::Open,
      pos,
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
          vec.iter()
            .find(|x| x.namespace.map_or(true, |ns| ns == export_namespace) && x.out_name == *export_name)
            .map(|name_match| name_match.in_name.clone())
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
    let filename = match &tail[0].value {
      ASTF::Atom(Literal::String(s)) => ImportDecl::parse_path_param(s).ok_or_else(|| {
        ImportDeclParseError::InvalidPath(s.clone())
      }),
      _ => Err(ImportDeclParseError::BadFilename(tail[0].clone())),
    }?;
    match tail.len() {
      0 => { unreachable!() } // We checked tail.is_empty() already
      1 => {
        // (1) Qualified import
        Ok(ImportDecl::named(filename, None, tail[0].pos))
      }
      2 => {
        match &tail[1].value {
          ASTF::Atom(Literal::Symbol(open)) if open == "open" => {
            // (5) Wildcard import
            Ok(ImportDecl::open(filename, tail[0].pos))
          }
          ASTF::Atom(Literal::Nil) | ASTF::Cons(_, _) => {
            // (3) or (4) Explicit import (possibly aliased)
            let imports: Vec<_> = DottedExpr::new(tail[1]).try_into().map_err(|_| invalid_ending_err(&tail[1..], tail[1].pos))?;
            let imports = imports.into_iter()
              .map(ImportName::<Option<Namespace>>::parse)
              .collect::<Result<Vec<_>, _>>()?;
            Ok(ImportDecl::restricted(filename, imports, tail[0].pos))
          }
          _ => {
            Err(invalid_ending_err(&tail[1..], tail[1].pos))
          }
        }
      }
      3 => {
        // (2) Qualified import (aliased)
        if tail[1].value != ASTF::symbol("as") {
          return Err(invalid_ending_err(&tail[1..], tail[1].pos));
        }
        match &tail[2].value {
          ASTF::Atom(Literal::Symbol(s)) => Ok(ImportDecl::named(filename, Some(s.clone()), tail[0].pos)),
          _ => Err(invalid_ending_err(&tail[1..], tail[1].pos))
        }
      }
      _ => {
        Err(invalid_ending_err(&tail[1..], tail[1].pos))
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

  fn symbol_to_namespace(symbol: &str, pos: SourceOffset) -> Result<Namespace, ImportDeclParseError> {
    match symbol {
      "value" => Ok(Namespace::Value),
      "function" => Ok(Namespace::Function),
      _ => Err(ImportDeclParseError::MalformedFunctionImport(AST::new(ASTF::symbol(symbol), pos))),
    }
  }

  pub fn parse(clause: &AST) -> Result<ImportName<Option<Namespace>>, ImportDeclParseError> {
    match &clause.value {
      ASTF::Atom(Literal::Symbol(s)) => {
        Ok(ImportName::simple(None, s.clone()))
      }
      ASTF::Cons(_, _) => {
        let vec: Vec<_> = DottedExpr::new(clause).try_into().map_err(|_| ImportDeclParseError::MalformedFunctionImport(clause.clone()))?;
        let shape_vec: Option<Vec<&str>> = vec
          .into_iter()
          .map(|x| x.as_symbol_ref())
          .collect();
        match shape_vec.as_deref() {
          Some([o, "as", i]) => {
            Ok(ImportName::new(None, (*i).to_owned(), (*o).to_owned()))
          }
          Some([o, ns, "as", i]) => {
            let ns = ImportName::<Option<Namespace>>::symbol_to_namespace(ns, clause.pos)?;
            Ok(ImportName::new(Some(ns), (*i).to_owned(), (*o).to_owned()))
          }
          Some([o, ns]) => {
            let ns = ImportName::<Option<Namespace>>::symbol_to_namespace(ns, clause.pos)?;
            Ok(ImportName::new(Some(ns), (*o).to_owned(), (*o).to_owned()))
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

  pub fn to_imported_id<'a>(&'a self) -> Box<dyn IdLike<NS=Namespace> + 'a> {
    Id::build(self.namespace, &self.in_name)
  }

  pub fn to_exported_id<'a>(&'a self) -> Box<dyn IdLike<NS=Namespace> + 'a> {
    Id::build(self.namespace, &self.out_name)
  }

}

fn invalid_ending_err(tail: &[&AST], pos: SourceOffset) -> ImportDeclParseError {
  let ending: Vec<AST> = tail.iter().map(|x| (*x).clone()).collect();
  ImportDeclParseError::InvalidEnding(AST::dotted_list(ending, AST::nil(pos)))
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

impl Error for ImportDeclParseError {}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::AST_PARSER;

  fn parse_ast(input: &str) -> AST {
    AST_PARSER.parse(input).unwrap()
  }

  fn parse_import(input: &str) -> Result<ImportDecl, ImportDeclParseError> {
    let ast = parse_ast(input);
    let dotted: Vec<_> = DottedExpr::new(&ast).try_into().unwrap();
    ImportDecl::parse(&dotted)
  }

  fn str_to_rpathbuf(input: &str) -> RPathBuf {
    RPathBuf::try_from(String::from(input)).unwrap()
  }

  fn so(x: usize) -> SourceOffset {
    SourceOffset(x)
  }

  #[test]
  fn default_import_name_test() {
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("/a/b/c")), "c");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("/abcd")), "abcd");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://foo/bar")), "bar");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://foo/bar.lisp")), "bar");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://foo/bar.gd")), "bar");
    assert_eq!(ImportDecl::default_import_name(&str_to_rpathbuf("res://")), "/"); // Degenerate case
  }

  #[test]
  fn test_parsing() {
    assert_eq!(parse_import(r#"("res://foo/bar")"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), None, so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar")"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), Some(String::from("bar")), so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" as foo)"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), Some(String::from("foo")), so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" as foo.baz)"#).unwrap(),
               ImportDecl::named(str_to_rpathbuf("res://foo/bar"), Some(String::from("foo.baz")), so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" open)"#).unwrap(),
               ImportDecl::open(str_to_rpathbuf("res://foo/bar"), so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" (a b))"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(ImportName::simple(None, String::from("a")),
                                           ImportName::simple(None, String::from("b"))),
                                      so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" ())"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(),
                                      so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" ((a as a1) b))"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(ImportName::new(None, String::from("a1"), String::from("a")),
                                           ImportName::simple(None, String::from("b"))),
                                      so(1)));
    assert_eq!(parse_import(r#"("res://foo/bar" ((a function as a1) (b value)))"#).unwrap(),
               ImportDecl::restricted(str_to_rpathbuf("res://foo/bar"),
                                      vec!(ImportName::new(Some(Namespace::Function), String::from("a1"), String::from("a")),
                                           ImportName::simple(Some(Namespace::Value), String::from("b"))),
                                      so(1)));
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
  }

}
