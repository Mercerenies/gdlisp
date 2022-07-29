
extern crate gdlisp;

use gdlisp::ir::identifier::ClassNamespace;
use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::{parse_compile_decl, parse_compile_decl_err};

#[test]
pub fn empty_file_test() {
  assert_eq!(parse_compile_decl("()"), "extends Reference\nstatic func run():\n    return null\n");
}

#[test]
pub fn simple_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) x))"),
             "extends Reference\nstatic func foo(x):\n    return x\nstatic func run():\n    return null\n");
}

#[test]
pub fn lambda_in_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () x) x))"), r#"extends Reference
class _LambdaBlock extends GDLisp.Function:
    var x
    func _init(x):
        self.x = x
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        return x
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
static func foo(x):
    return x
static func run():
    return null
"#);
}

#[test]
pub fn closed_rw_in_function_declaration_test() {
  assert_eq!(parse_compile_decl("((defn foo (x) (lambda () (set x 1)) x))"), r#"extends Reference
class _LambdaBlock extends GDLisp.Function:
    var x
    func _init(x):
        self.x = x
        self.__gdlisp_required = 0
        self.__gdlisp_optional = 0
        self.__gdlisp_rest = 0
    func call_func():
        x.contents = 1
        return x.contents
    func call_funcv(args):
        if args == null:
            return call_func()
        else:
            push_error("Too many arguments")
static func foo(x):
    x = GDLisp.Cell.new(x)
    return x.contents
static func run():
    return null
"#);
}

#[test]
pub fn mutually_recursive_test() {
  assert_eq!(parse_compile_decl("((defn foo () (bar)) (defn bar () (foo)))"), r#"extends Reference
static func foo():
    return bar()
static func bar():
    return foo()
static func run():
    return null
"#);
}

#[test]
pub fn nonexistent_function_test() {
  assert_eq!(
    parse_compile_decl_err("((defn foo () (bar)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("bar")), SourceOffset(14)))),
  );
}

#[test]
pub fn progn_decl_test() {
  assert_eq!(parse_compile_decl("((progn (progn (defn foo () ()) (defn bar () ()))))"), r#"extends Reference
static func foo():
    return null
static func bar():
    return null
static func run():
    return null
"#);
}

#[test]
pub fn declare_value_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare value x) x)"), r#"extends Reference
static func run():
    return x
"#);
}

#[test]
pub fn declare_value_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal x) x)"), r#"extends Reference
static func run():
    return x
"#);
}

#[test]
pub fn declare_value_test_3() {
  assert_eq!(parse_compile_decl("((sys/declare superglobal x) (defconst y x))"), r#"extends Reference
const y = x
static func run():
    return null
"#);
}

#[test]
pub fn declare_value_non_const_test() {
  assert_eq!(
    parse_compile_decl_err("((sys/declare value x) (defconst y x))"),
    Err(PError::from(GDError::new(GDErrorF::NotConstantEnough(String::from("y")), SourceOffset(35)))),
  );
}

#[test]
pub fn declare_function_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare function f ()) (f))"), r#"extends Reference
static func run():
    return f()
"#);
}

#[test]
pub fn declare_function_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare function f (a &opt b)) (f 1) (f 1 2))"), r#"extends Reference
static func run():
    f(1, null)
    return f(1, 2)
"#);
}

#[test]
pub fn declare_function_test_3() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction f (a &opt b)) (f 1) (f 1 2))"), r#"extends Reference
static func run():
    f(1, null)
    return f(1, 2)
"#);
}

#[test]
pub fn nonsense_modifier_function_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () public private 1))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(21)))),
  );
}

#[test]
pub fn declare_function_inner_test_1() {
  assert_eq!(parse_compile_decl("((sys/declare function f ()) (defclass Foo (Reference) (defn _init () (f))))"),
             r#"extends Reference
class Foo extends Reference:
    func _init():
        __gdlisp_outer_class_0.f()
    var __gdlisp_outer_class_0 = load("res://TEST.gd")
static func run():
    return null
"#);
}

#[test]
pub fn declare_function_inner_test_2() {
  assert_eq!(parse_compile_decl("((sys/declare superfunction f ()) (defclass Foo (Reference) (defn _init () (f))))"),
             r#"extends Reference
class Foo extends Reference:
    func _init():
        f()
static func run():
    return null
"#);
}

#[test]
pub fn duplicate_const_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defconst A 1) (defconst A 1))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(17)))),
  );
}

#[test]
pub fn duplicate_const_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defconst A 1) (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(34)))),
  );
}

#[test]
pub fn duplicate_var_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defvar A 1) (defvar A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(32)))),
  );
}

#[test]
pub fn duplicate_var_const_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defvar A 1) (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(32)))),
  );
}

#[test]
pub fn duplicate_fn_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defn foo ()) (defn foo ())))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(33)))),
  );
}

#[test]
pub fn duplicate_signal_in_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defclass Foo () (defsignal foo) (defsignal foo ())))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Signal, String::from("foo")), SourceOffset(35)))),
  );
}

#[test]
pub fn duplicate_fn_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defn foo () 1) (defn foo () 2))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(18)))),
  );
}

#[test]
pub fn overlapping_namespaces_test() {
  assert_eq!(parse_compile_decl("((defn foo ()) (defconst foo 1))"),
             r#"extends Reference
static func foo():
    return null
const foo = 1
static func run():
    return null
"#);
}

#[test]
pub fn overlapping_namespaces_in_class_test() {
  assert_eq!(parse_compile_decl("((defclass Foo () (defsignal foo) (defn foo ()) (defconst foo 1)))"),
             r#"extends Reference
class Foo extends Reference:
    func _init():
        pass
    signal foo
    func foo():
        return null
    const foo = 1
static func run():
    return null
"#);
}
#[test]
pub fn duplicate_const_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((new Reference (defconst A 1) (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(32)))),
  );
}

#[test]
pub fn duplicate_var_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((new Reference (defvar A 1) (defvar A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(30)))),
  );
}

#[test]
pub fn duplicate_var_const_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((new Reference (defvar A 1) (defconst A 1)))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Value, String::from("A")), SourceOffset(30)))),
  );
}

#[test]
pub fn duplicate_fn_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((new Reference (defn foo ()) (defn foo ())))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(31)))),
  );
}

#[test]
pub fn duplicate_signal_in_lambda_class_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((new Reference (defsignal foo) (defsignal foo ())))"#),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Signal, String::from("foo")), SourceOffset(33)))),
  );
}
