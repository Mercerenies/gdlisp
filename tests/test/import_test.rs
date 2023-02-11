
extern crate gdlisp;

use super::common::import::MockFileLoader;
use super::common::dummy_config;

use gdlisp::ir::identifier::{Id, Namespace};
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::Pipeline;
use gdlisp::pipeline::error::PError;
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
  let result = pipeline.load_file("main.lisp", SourceOffset(0))?.gdscript.to_gd();
  Ok(result)
}

#[test]
fn qualified_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp")
    (defn run ()
      (example/one)
      (example/two))
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
    (defn run ()
      (example-name/one)
      (example-name/two))
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
    (defn run ()
      (one))
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
      (defn run ()
        (two))
    "#),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("two")), SourceOffset(67)))),
  );
}

#[test]
fn restricted_import_alias_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp" ((one as my-one)))
    (defn run ()
      (my-one))
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
      (defn run ()
        (one))
    "#),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("one")), SourceOffset(79)))),
  );
}

#[test]
fn open_import_test() {
  assert_eq!(load_and_output_simple_file(r#"
    (use "res://example.lisp" open)
    (defn run ()
      (one)
      (two))
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
    (defn run ()
      (f 43))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")


static func f(x):
    return _Import_0.add_one(x)


static func run():
    return 44
"#);
}

#[test]
fn import_declared_value_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(sys/declare value a public)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defn f (x) a)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")


static func f(x):
    return _Import_0.a
"#);
}

#[test]
fn import_declared_constant_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(sys/declare constant a public)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defconst y a)
    (defn f (x) a)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")
const y = _Import_0.a


static func f(x):
    return _Import_0.a
"#);
}

#[test]
fn import_declared_superglobal_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(sys/declare superglobal a public)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defconst y a)
    (defn f (x) a)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")
const y = a


static func f(x):
    return a
"#);
}

#[test]
fn import_declared_function_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(sys/declare function a () public)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defn f (x) (a))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")


static func f(x):
    return _Import_0.a()
"#);
}

#[test]
fn import_declared_superfunction_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(sys/declare superfunction a () public)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defn f (x) (a))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")


static func f(x):
    return a()
"#);
}

#[test]
fn imported_name_to_class_var_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(sys/declare value A public) (defconst B 100)");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (defclass Main (Reference) main
      (defvar a A)
      (defvar b B)
      (defvar a1 A onready)
      (defvar b1 B onready))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Reference


const _Import_0 = preload("res://example.gd")


func _init():
    self.a = _Import_0.A


var a
var b = _Import_0.B
var a1
onready var b1 = _Import_0.B


func _ready():
    self.a1 = _Import_0.A
"#);
}

#[test]
fn cyclic_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(use \"res://b.lisp\" open) (defn foo (x) (bar))");
  loader.add_file("b.lisp", "(use \"res://a.lisp\" open) (defn bar (x) (foo))");
  loader.add_file("main.lisp", r#"
    (use "res://b.lisp" open)
    (defn run ()
      (bar))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0));
  assert_eq!(result.map(|_| ()),
             Err(PError::from(
               GDError::new(GDErrorF::CyclicImport(String::from("./b.lisp")), SourceOffset(0)),
             )));
}

#[test]
fn macro_uses_preload_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn add-one (x) (+ x 1))");
  // Note: We have to explicitly import the file, even if we later
  // load it using `preload`. Macro expansion only uses imports for
  // resolution. I may loosen this constraint later, but for right now
  // it is required.
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp")
    (defmacro f (x) ((preload "res://example.lisp"):add-one x))
    (defn run ()
      (f 43))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const example_0 = preload("res://example.gd")


static func f(x):
    return preload("res://example.gd").add_one(x)


static func run():
    return 44
"#);
}

#[test]
fn macro_uses_preload_without_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn add-one (x) (+ x 1))");
  loader.add_file("main.lisp", r#"
    (defmacro f (x) ((preload "res://example.lisp"):add-one x))
    (defn run ()
      (f 43))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0));
  let expected_error_path = String::from("res://example.gd");
  assert_eq!(result.map(|_| ()),
             Err(PError::from(
               GDError::new(GDErrorF::NoSuchFile(expected_error_path), SourceOffset(22)),
             )));
}

#[test]
fn symbol_macro_uses_other_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("example.lisp", "(defn add-one (x) (+ x 1))");
  loader.add_file("main.lisp", r#"
    (use "res://example.lisp" open)
    (define-symbol-macro x (add-one 2))
    (defn run ()
      x)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://example.gd")


static func __gdlisp_SymbolMacroFunction_x():
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
    (defn run ()
      (add-one 43))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      (go))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      (go))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      x)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      go)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      go)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      (c/g 3))
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    (defn run ()
      c/g)
  "#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) (defn run () Foo)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) (defn run () [Foo VALUE])"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) (defn run () a)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("a")), SourceOffset(39)))),
  );
}

#[test]
fn import_declare_test_failed_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(sys/declare value a)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (a))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("a") }), SourceOffset(5)))),
  );
}

#[test]
fn public_fn_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () public 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp") (defn run () (a/foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const a_0 = preload("res://a.gd")
"#);
}

#[test]
fn private_fn_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")
"#);
}

#[test]
fn private_fn_import_test_3() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" open) (defn run () (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("foo")), SourceOffset(39)))),
  );
}

#[test]
fn private_fn_import_test_4() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_macro_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defmacro foo () public 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")
"#);
}

#[test]
fn private_macro_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defmacro foo () private 1)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_const_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defconst foo 1 public)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")
"#);
}

#[test]
fn private_const_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defconst foo 1 private)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_enum_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defenum foo public A B)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")
"#);
}

#[test]
fn private_enum_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defenum foo private A B)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn public_class_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) public (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")
"#);
}

#[test]
fn public_class_import_test_2() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) main public (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")
"#);
}

#[test]
fn private_class_import_test_1() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass foo (Node) private (defvar example 1))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  assert_eq!(
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
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
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
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
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
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
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::UnknownImportedName(Id { namespace: Namespace::Function, name: String::from("foo") }), SourceOffset(5)))),
  );
}

#[test]
fn lazy_val_import_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(deflazy foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo)) (defn run () foo)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo)) (defmacro bar () foo) (defn run () (bar))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
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
    pipeline.load_file("main.lisp", SourceOffset(0)).map(|_| ()),
    Err(PError::from(GDError::new(GDErrorF::AmbiguousNamespace(String::from("foo")), SourceOffset(5)))),
  );
}

#[test]
fn ambiguous_import_namespace_disambiguate_value_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo ()) (defconst foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" ((foo value))) (defn bar () foo)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")


static func bar():
    return _Import_0.foo
"#);
}

#[test]
fn ambiguous_import_namespace_disambiguate_function_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defn foo ()) (defconst foo 10)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" ((foo function))) (defn bar () (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")


static func bar():
    return _Import_0.foo()
"#);
}

#[test]
fn inner_class_extends_inner_class_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass A (Reference))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (A)) (defclass B (A))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")


class B extends _Import_0.A:

    func _init():
        pass
"#);
}

#[test]
fn inner_class_extends_main_class_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass A (Reference) main)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (A)) (defclass B (A))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends Node


const _Import_0 = preload("res://a.gd")


class B extends _Import_0:

    func _init():
        pass
"#);
}

#[test]
fn main_class_extends_inner_class_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass A (Reference))");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (A)) (defclass B (A) main)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends "res://a.gd".A


const _Import_0 = preload("res://a.gd")


func _init():
    pass
"#);
}

#[test]
fn main_class_extends_main_class_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", "(defclass A (Reference) main)");
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (A)) (defclass B (A) main)"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap().gdscript.to_gd();
  assert_eq!(result, r#"extends "res://a.gd"


const _Import_0 = preload("res://a.gd")


func _init():
    pass
"#);
}

#[test]
fn load_non_gdlisp_resource_in_macro_test() {
  let mut loader = MockFileLoader::new();
  loader.add_file("a.lisp", r#"(use "res://some_resource.png") (defmacro foo () some_resource)"#);
  loader.add_file("main.lisp", r#"(use "res://a.lisp" (foo)) (defn bar () (foo))"#);
  let mut pipeline = Pipeline::with_resolver(dummy_config(), Box::new(loader));
  let result = pipeline.load_file("main.lisp", SourceOffset(0)).unwrap_err();
  assert_eq!(result, PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("some_resource")),
                                               SourceOffset(49))));
}
