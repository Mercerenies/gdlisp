
extern crate gdlisp;

use super::common::import::MockFileLoader;

use gdlisp::pipeline::loader::FileLoader;

fn setup_simple_file_loader(loader: &mut MockFileLoader) {
  loader.add_file("example.lisp", "(defn one () 1) (defn two () 2)");
}

fn load_and_output_simple_file(input: &str) -> String {
  let mut loader = MockFileLoader::new();
  setup_simple_file_loader(&mut loader);
  loader.add_file("main.lisp", input);
  loader.load_file("main.lisp").unwrap().gdscript().to_gd()
}

#[test]
fn qualified_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp")
    (example.one)
    (example.two)
  "#), r#"extends Node
const example_0 = preload("res://example.gd")
static func run():
    example_0.one()
    return example_0.two()
"#);
}

#[test]
fn aliased_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp" as example-name)
    (example-name.one)
    (example-name.two)
  "#), r#"extends Node
const example_name_0 = preload("res://example.gd")
static func run():
    example_name_0.one()
    return example_name_0.two()
"#);
}

#[test]
fn restricted_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp" (one))
    (one)
  "#), r#"extends Node
const _Import_0 = preload("res://example.gd")
static func run():
    return _Import_0.one()
"#);
}

#[test]
#[should_panic]
fn restricted_import_test_failed() {
  load_and_output_simple_file(r#"
    (use "res://example.lisp" (one))
    (two)
  "#);
}

#[test]
fn restricted_import_alias_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp" ((one as my-one)))
    (my-one)
  "#), r#"extends Node
const _Import_0 = preload("res://example.gd")
static func run():
    return _Import_0.one()
"#);
}

#[test]
#[should_panic]
fn restricted_import_alias_test_failed() {
  load_and_output_simple_file(r#"
    (use "res://example.lisp" ((one as my-one)))
    (one)
  "#);
}

#[test]
fn open_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp" open)
    (one)
    (two)
  "#), r#"extends Node
const _Import_0 = preload("res://example.gd")
static func run():
    _Import_0.one()
    return _Import_0.two()
"#);
}

#[test]
#[should_panic]
fn nonexistent_import_test() {
  load_and_output_simple_file(r#"
    (use "res://example.lisp" (nonexistent-function-name))
  "#);
}
