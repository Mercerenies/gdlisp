
use super::common::dummy_pipeline;

use gdlisp::ir;
use gdlisp::ir::declaration_table::DeclarationTable;
use gdlisp::ir::depends::Dependencies;
use gdlisp::ir::identifier::{IdLike, Id, Namespace};
use gdlisp::AST_PARSER;
use gdlisp::pipeline::source::SourceOffset;

use std::collections::{HashMap, HashSet};

// TODO Test some dependency analysis that involves importing multiple files.

fn dependencies_of<'a>(input: &str, target_name: &(dyn IdLike<NS=Namespace> + 'a), pos: SourceOffset) -> Dependencies {
  let ast = AST_PARSER.parse(input).unwrap();
  let (toplevel, _macros) = ir::compile_and_check(&mut dummy_pipeline(), &ast).unwrap();
  ir::scope::check_scopes(&toplevel).unwrap();
  let table = DeclarationTable::from(toplevel.decls);
  Dependencies::identify(&table, &HashSet::new(), target_name, pos)
}

fn make_deps(known: Vec<(&str, usize)>, unknown: Vec<(&str, usize)>) -> Dependencies {
  Dependencies {
    known: known.into_iter().map(|(x, p)| (Id::new(Namespace::Function, x.to_owned()), SourceOffset::from(p))).collect(),
    imports: HashMap::new(),
    unknown: unknown.into_iter().map(|(x, p)| (Id::new(Namespace::Function, x.to_owned()), SourceOffset::from(p))).collect(),
  }
}

#[test]
pub fn dependencies_test_empty() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () ())
     (defn baz () ()))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(10)),
             make_deps(vec!(("baz", 10)), vec!()));
}

#[test]
pub fn dependencies_test_forward() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () ())
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(999)),
             make_deps(vec!(("baz", 999), ("bar", 63)), vec!()));
}

#[test]
pub fn dependencies_test_backward() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () (baz))
     (defn baz () ()))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(9)),
             make_deps(vec!(("baz", 9)), vec!()));
}

#[test]
pub fn dependencies_test_two() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () ())
     (defn bar () ())
     (defn baz () (foo) (bar)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(0)),
             make_deps(vec!(("baz", 0), ("bar", 69), ("foo", 63)), vec!()));
}

#[test]
pub fn dependencies_test_transitive() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (bar))
     (defn bar () ())
     (defn baz () (foo)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(0)),
             make_deps(vec!(("baz", 0), ("bar", 19), ("foo", 66)), vec!()));
}

#[test]
pub fn dependencies_test_recursion() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (bar))
     (defn bar () (bar))
     (defn baz () (foo) (baz)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(999)),
             make_deps(vec!(("baz", 75), ("bar", 19), ("foo", 69)), vec!()));
}

#[test]
pub fn dependencies_test_cycle() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (baz))
     (defn bar () (foo))
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(999)),
             make_deps(vec!(("baz", 19), ("bar", 69), ("foo", 44)), vec!()));
}

#[test]
pub fn dependencies_test_unknowns_1() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (aaa))
     (defn bar () (foo))
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(999)),
             make_deps(vec!(("baz", 999), ("bar", 69), ("foo", 44)), vec!(("aaa", 19))));
}

#[test]
pub fn dependencies_test_unknowns_2() {
  assert_eq!(dependencies_of(r#"
    ((defn foo () (aaa))
     (defn bar () (bbb))
     (defn baz () (bar)))"#, &*Id::build(Namespace::Function, "baz"), SourceOffset(999)),
             make_deps(vec!(("baz", 999), ("bar", 69)), vec!(("bbb", 44))));
}
