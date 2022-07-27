
use super::common::*;

#[test]
pub fn translate_local_vars_test_1() {
  assert_eq!(parse_compile_decl("((defn foo () (let ((x 1)) x)))"),
             r#"extends Reference
static func foo():
    var x_0 = 1
    return x_0
static func run():
    return null
"#);
}

#[test]
pub fn translate_local_vars_test_2() {
  assert_eq!(parse_compile_decl("((defn foo () (let* ((x 1) (x 2)) x)))"),
             r#"extends Reference
static func foo():
    var x_0 = 1
    var x_1 = 2
    return x_1
static func run():
    return null
"#);
}

#[test]
pub fn translate_local_vars_test_3() {
  assert_eq!(parse_compile_decl("((defn foo () (let* ((x-y 1) (x-y 2)) x-y)))"),
             r#"extends Reference
static func foo():
    var x_y_0 = 1
    var x_y_1 = 2
    return x_y_1
static func run():
    return null
"#);
}

#[test]
pub fn translate_arguments_test() {
  assert_eq!(parse_compile_decl("((defn foo (a-b) a-b))"),
             r#"extends Reference
static func foo(a_b):
    return a_b
static func run():
    return null
"#);
}

#[test]
pub fn translate_constructor_arguments_test_1() {
  assert_eq!(parse_compile_decl("((defclass Foo (Reference) (defn _init (a-b))))"),
             r#"extends Reference
class Foo extends Reference:
    func _init(a_b):
        pass
static func run():
    return null
"#);
}

#[test]
pub fn translate_constructor_arguments_test_2() {
  assert_eq!(parse_compile_decl("((defclass Foo (Reference) (defn _init (a-b) (super a-b))))"),
             r#"extends Reference
class Foo extends Reference:
    func _init(a_b).(a_b):
        pass
static func run():
    return null
"#);
}

#[test]
pub fn translate_function_name_test() {
  assert_eq!(parse_compile_decl("((defn foo-bar ()))"),
             r#"extends Reference
static func foo_bar():
    return null
static func run():
    return null
"#);
}

#[test]
pub fn translate_macro_name_test() {
  assert_eq!(parse_compile_decl("((defmacro foo-bar ()))"),
             r#"extends Reference
static func foo_bar():
    return null
static func run():
    return null
"#);
}

#[test]
pub fn translate_const_name_test() {
  assert_eq!(parse_compile_decl("((defconst FOO-BAR 1))"),
             r#"extends Reference
const FOO_BAR = 1
static func run():
    return null
"#);

}

#[test]
pub fn translate_class_name_test() {
  assert_eq!(parse_compile_decl("((defclass Foo-Bar (Reference)))"),
             r#"extends Reference
class Foo_Bar extends Reference:
    func _init():
        pass
static func run():
    return null
"#);
}

#[test]
pub fn translate_enum_name_test_1() {
  assert_eq!(parse_compile_decl("((defenum Foo-Bar A-B))"),
             r#"extends Reference
enum Foo_Bar {
    A_B,
}
static func run():
    return null
"#);
}

#[test]
pub fn translate_enum_name_test_2() {
  assert_eq!(parse_compile_decl("((defenum Foo-Bar (A-B 1)))"),
             r#"extends Reference
enum Foo_Bar {
    A_B = 1,
}
static func run():
    return null
"#);
}

#[test]
pub fn translate_declare_name_test() {
  assert_eq!(parse_compile_decl("((sys/declare function foo-bar ()) (foo-bar))"),
             r#"extends Reference
static func run():
    return foo_bar()
"#);
}
