
extern crate gdlisp;

// TODO Lazy loading in NamedFileServer so these tests don't have to be #[ignore]

use super::common::import::MockFileLoader;
use super::common::dummy_config;

use gdlisp::pipeline::Pipeline;

fn setup_simple_file_loader(loader: &mut MockFileLoader) {
  loader.add_file("example.lisp", "(defn one () 1) (defn two () 2)");
}

fn load_and_output_simple_file(input: &str) -> String {
  let mut loader = MockFileLoader::new();
  setup_simple_file_loader(&mut loader);
  loader.add_file("main.lisp", input);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  pipeline.load_file("main.lisp").unwrap().gdscript.to_gd()
}

#[test]
#[ignore]
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
#[ignore]
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
#[ignore]
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
#[ignore]
#[should_panic]
fn restricted_import_test_failed() {
  load_and_output_simple_file(r#"
    (use "res://example.lisp" (one))
    (two)
  "#);
}

#[test]
#[ignore]
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
#[ignore]
#[should_panic]
fn restricted_import_alias_test_failed() {
  load_and_output_simple_file(r#"
    (use "res://example.lisp" ((one as my-one)))
    (one)
  "#);
}

#[test]
#[ignore]
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
#[ignore]
#[should_panic]
fn nonexistent_import_test() {
  load_and_output_simple_file(r#"
    (use "res://example.lisp" (nonexistent-function-name))
  "#);
}

#[test]
#[ignore]
fn macro_uses_other_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn add-one (x) (+ x 1))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defmacro f (x) (add-one x))
    (f 43)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://example.gd")
static func f(x_1):
    return _Import_0.add_one(x_1)
static func run():
    return 44
"#);
}

#[test]
#[ignore]
fn macro_from_other_file_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defmacro add-one (x) (+ x 1))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (add-one 43)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://example.gd")
static func run():
    return 44
"#);
}

#[test]
#[ignore]
fn macro_several_files_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn add-one-f (x) (+ x 1))");
  loader.add_file("b.lisp", r#"(use "res://a.lisp" open) (defmacro f (x) (add-one-f x))"#);
  loader.add_file("c.lisp", r#"(use "res://b.lisp" open) (defmacro g (x) (+ x (f 87)))"#);
  loader.add_file("main.lisp", r#"
    (use "res://c.lisp")
    (c.g 3)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const c_0 = preload("res://c.gd")
static func run():
    return 91
"#);
}
