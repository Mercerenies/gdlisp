
extern crate gdlisp;

use super::common::import::MockFileLoader;
use super::common::dummy_config;

use gdlisp::ir::identifier::{Id, Namespace};
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

fn setup_simple_file_loader(loader: &mut MockFileLoader) {
  loader.add_file("example.lisp", "(defn one () 1) (defn two () 2)");
}

fn load_and_output_simple_file(input: &str) -> String {
  load_and_output_simple_file_err(input).unwrap()
}

fn load_and_output_simple_file_err(input: &str) -> Result<String, PError> {
  let mut loader = MockFileLoader::new();
  setup_simple_file_loader(&mut loader);
  loader.add_file("main.lisp", input);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp")?.gdscript.to_gd();
  Ok(result)
}

#[test]
fn qualified_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp")
    (example/one)
    (example/two)
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
    (example-name/one)
    (example-name/two)
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
fn restricted_import_test_failed() {
  assert_eq!(
    load_and_output_simple_file_err(r#"
      (use "res://example.lisp" (one))
      (two)
    "#),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("two")), SourceOffset(46)))),
  );
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
fn restricted_import_alias_test_failed() {
  assert_eq!(
    load_and_output_simple_file_err(r#"
      (use "res://example.lisp" ((one as my-one)))
      (one)
    "#),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("one")), SourceOffset(58)))),
  );
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
fn nonexistent_import_test() {
  assert_eq!(
    load_and_output_simple_file_err(r#"
      (use "res://example.lisp" (nonexistent-function-name))
    "#),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function,
                                                                     name: String::from("nonexistent-function-name") }),
                                  SourceOffset(12)))),
  );
}

#[test]
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
fn symbol_macro_uses_other_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn add-one (x) (+ x 1))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (define-symbol-macro x (add-one 2))
    x
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://example.gd")
static func x():
    return _Import_0.add_one(2)
static func run():
    return 3
"#);
}

#[test]
fn macro_from_other_file_import_test_1() {
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
fn macro_from_other_file_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn outer () 44) (defclass Foo (Reference) (defn go () (outer))) (defmacro go () ((Foo:new):go))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (go)
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
fn macro_from_other_file_import_test_3() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn outer () 44) (defclass Foo (Reference) (defn go () static (outer))) (defmacro go () (Foo:go))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (go)
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
fn symbol_macro_from_other_file_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(define-symbol-macro x 10)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    x
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://example.gd")
static func run():
    return 10
"#);
}

#[test]
fn symbol_macro_from_other_file_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn outer () 44) (defclass Foo (Reference) (defn go () (outer))) (define-symbol-macro go ((Foo:new):go))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    go
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
fn symbol_macro_from_other_file_import_test_3() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn outer () 44) (defclass Foo (Reference) (defn go () static (outer))) (define-symbol-macro go (Foo:go))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    go
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
fn macro_several_files_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn add-one-f (x) (+ x 1))");
  loader.add_file("b.lisp", r#"(use "res://a.lisp" open) (defmacro f (x) (add-one-f x))"#);
  loader.add_file("c.lisp", r#"(use "res://b.lisp" open) (defmacro g (x) (+ x (f 87)))"#);
  loader.add_file("main.lisp", r#"
    (use "res://c.lisp")
    (c/g 3)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const c_0 = preload("res://c.gd")
static func run():
    return 91
"#);
}

#[test]
fn symbol_macro_several_files_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn add-one-f (x) (+ x 1))");
  loader.add_file("b.lisp", r#"(use "res://a.lisp" open) (define-symbol-macro f (add-one-f 3))"#);
  loader.add_file("c.lisp", r#"(use "res://b.lisp" open) (define-symbol-macro g f)"#);
  loader.add_file("main.lisp", r#"
    (use "res://c.lisp")
    c/g
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const c_0 = preload("res://c.gd")
static func run():
    return 4
"#);
}

#[test]
fn main_class_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass Foo (Reference) main)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) Foo"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return _Import_0
"#);
}

#[test]
fn main_class_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass Foo (Reference) main) (defconst VALUE 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) [Foo VALUE]"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return [_Import_0, _Import_0.VALUE]
"#);
}

#[test]
fn import_declare_test_failed_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(sys/declare value a)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) a"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("a")), SourceOffset(26)))),
  );
}

#[test]
fn import_declare_test_failed_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(sys/declare value a)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (a))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("a") }), SourceOffset(5)))),
  );
}

#[test]
fn public_fn_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () public 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp") (a/foo)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const a_0 = preload("res://a.gd")
static func run():
    return a_0.foo()
"#);
}

#[test]
fn private_fn_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp")"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const a_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn private_fn_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn private_fn_import_test_3() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) (foo)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("foo")), SourceOffset(26)))),
  );
}

#[test]
fn private_fn_import_test_4() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_macro_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defmacro foo () public 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn private_macro_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defmacro foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_const_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defconst foo 1 public)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn private_const_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defconst foo 1 private)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_enum_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defenum foo public A B)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn private_enum_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defenum foo private A B)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_class_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) public (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn public_class_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) main public (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return null
"#);
}

#[test]
fn private_class_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) private (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn private_class_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) main private (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn private_lazy_val_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(deflazy foo 10 private)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn private_symbol_macro_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(define-symbol-macro foo 10 private)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn lazy_val_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(deflazy foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo)) foo"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func run():
    return load("res://a.gd")._lazy_0()
"#);
}

#[test]
fn lazy_val_import_run_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(deflazy foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo)) (defmacro bar () foo) (bar)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func bar():
    return load("res://a.gd")._lazy_0()
static func run():
    return 10
"#);
}

#[test]
fn ambiguous_import_namespace_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo ()) (defconst foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp").map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::AmbiguousNamespace(String::from("foo")), SourceOffset(5)))),
  );
}

#[test]
fn ambiguous_import_namespace_disambiguate_value_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo ()) (defconst foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" ((foo value))) (defn bar () foo)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func bar():
    return _Import_0.foo
static func run():
    return null
"#);
}

#[test]
fn ambiguous_import_namespace_disambiguate_function_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo ()) (defconst foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" ((foo function))) (defn bar () (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp").unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node
const _Import_0 = preload("res://a.gd")
static func bar():
    return _Import_0.foo()
static func run():
    return null
"#);
}
