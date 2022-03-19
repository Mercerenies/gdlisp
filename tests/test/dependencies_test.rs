
use super::common::dummy_pipeline;

use gdlisp::ir;
use gdlisp::ir::declaration_table::DeclarationTable;
use gdlisp::ir::depends::Dependencies;
use gdlisp::ir::identifier::{IdLike, Id, Namespace};
use gdlisp::parser;

use std::collections::HashSet;

// TODO Test some dependency analysis that involves importing multiple files.

fn dependencies_of<'a>(input: &str, target_name: &(dyn IdLike<NS=Namespace> + 'a)) -> Dependencies {
  let parser = parser::ASTParser::new();
  let ast = parser.parse(input).unwrap();
  let (toplevel, _macros) = ir::compile_toplevel(&mut dummy_pipeline(), &ast).unwrap();
  ir::scope::check_scopes(&toplevel).unwrap();
  let table = DeclarationTable::from(toplevel.decls);
  Dependencies::identify(&table, &HashSet::new(), target_name)
}

fn make_deps(known: Vec<&str>, unknown: Vec<&str>) -> Dependencies {
  Dependencies {
    known: known.into_iter().map(|x| Id::new(Namespace::Function, x.to_owned())).collect(),
    imports: HashSet::new(),
    unknown: unknown.into_iter().map(|x| Id::new(Namespace::Function, x.to_owned())).collect(),
  }
}

#[test]
pub fn dependencies_test_empty() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () ())
     (defn baz () ()))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz"), vec!()));
}

#[test]
pub fn dependencies_test_forward() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () ())
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar"), vec!()));
}

#[test]
pub fn dependencies_test_backward() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () (baz))
     (defn baz () ()))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz"), vec!()));
}

#[test]
pub fn dependencies_test_two() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () ())
     (defn baz () (foo) (bar)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar", "foo"), vec!()));
}

#[test]
pub fn dependencies_test_transitive() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (bar))
     (defn bar () ())
     (defn baz () (foo)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar", "foo"), vec!()));
}

#[test]
pub fn dependencies_test_recursion() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (bar))
     (defn bar () (bar))
     (defn baz () (foo) (baz)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar", "foo"), vec!()));
}

#[test]
pub fn dependencies_test_cycle() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (baz))
     (defn bar () (foo))
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar", "foo"), vec!()));
}

#[test]
pub fn dependencies_test_unknowns_1() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (aaa))
     (defn bar () (foo))
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar", "foo"), vec!("aaa")));
}

#[test]
pub fn dependencies_test_unknowns_2() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (aaa))
     (defn bar () (bbb))
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz")),
             make_deps(vec!("baz", "bar"), vec!("bbb")));
}
