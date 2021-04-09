
extern crate gdlisp;

use super::common::{parse_compile_decl, parse_and_run, parse_compile_and_output};

#[test]
#[ignore]
pub fn simple_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) x))"), "extends Reference\nstatic func foo(x_0):\n    return x_0\nstatic func run():\n    return null\n")
}

#[test]
#[ignore]
pub fn constant_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo () 10) (foo))"), "extends Reference\nstatic func foo():\n    return 10\nstatic func run():\n    return 10\n");
}

#[test]
#[ignore]
pub fn arithmetic_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 100)) (foo 9))"), "extends Reference\nstatic func foo(x_0):\n    return x_0 + 100\nstatic func run():\n    return 109\n");
}

#[test]
#[ignore]
pub fn quote_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro my-quote (x) (cons 'quote (cons x ()))) (my-quote abc))"), "extends Reference\nstatic func my_quote(x_0):\n    return GDLisp.cons(GDLisp.intern(\"quote\"), GDLisp.cons(x_0, null))\nstatic func run():\n    return GDLisp.intern(\"abc\")\n");
}

#[test]
#[ignore]
pub fn macro_in_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar () (foo 0)) (bar))"), "extends Reference\nstatic func foo(x_0):\n    return x_0 + 1\nstatic func bar():\n    return 1\nstatic func run():\n    return 1\n");
}

#[test]
#[ignore]
pub fn macro_from_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar (x) (cons 'foo (cons x ()))) (bar 2))"), "extends Reference\nstatic func foo(x_0):\n    return x_0 + 1\nstatic func bar(x_1):\n    return GDLisp.cons(GDLisp.intern(\"foo\"), GDLisp.cons(x_1, null))\nstatic func run():\n    return 3\n");
}

#[test]
#[should_panic]
#[ignore]
pub fn bad_args_macro_test() {
  parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar (x) (cons 'foo (cons x ()))) (bar))");
}

#[test]
#[ignore]
pub fn rest_args_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (&rest x) x) (foo + 1 2))"), "extends Reference\nstatic func foo(x_0):\n    return x_0\nstatic func run():\n    return 1 + 2\n");
}

#[test]
#[ignore]
pub fn optional_args_macro_test_1() {
  assert_eq!(parse_compile_decl("((defmacro foo (&opt x) x) (foo 1))"), "extends Reference\nstatic func foo(x_0):\n    return x_0\nstatic func run():\n    return 1\n");
}

#[test]
#[ignore]
pub fn optional_args_macro_test_2() {
  assert_eq!(parse_compile_decl("((defmacro foo (&opt x) x) (foo))"), "extends Reference\nstatic func foo(x_0):\n    return x_0\nstatic func run():\n    return null\n");
}

#[test]
#[ignore]
pub fn to_decl_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo () '(defn bar () ())) (foo))"), "extends Reference\nstatic func foo():\n    return GDLisp.cons(GDLisp.intern(\"defn\"), GDLisp.cons(GDLisp.intern(\"bar\"), GDLisp.cons(null, GDLisp.cons(null, null))))\nstatic func bar():\n    return null\nstatic func run():\n    return null\n");
}

#[test]
#[ignore]
pub fn to_progn_macro_test() {
  assert_eq!(parse_and_run(r#"((defmacro foo () '(progn (defmacro bar (x) x) (print (bar 3)))) (foo))"#), "\n3\n");
}

#[test]
#[ignore]
pub fn macrolet_basic_test() {
  let result = parse_compile_and_output("(macrolet ((foo () 100)) (foo))");
  assert_eq!(result, "return 100\n");
}

#[test]
#[ignore]
pub fn macrolet_shadowing_test() {
  let result = parse_compile_and_output("(macrolet ((foo () 100)) [(foo) (macrolet ((foo () 99)) (foo)) (foo)])");
  assert_eq!(result, "return [100, 99, 100]\n");
}

#[test]
#[ignore]
pub fn macrolet_global_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) [(foo) (macrolet ((foo () 99)) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func run():
    return [100, 99, 100]
"#);
}

#[test]
#[ignore]
pub fn macrolet_global_function_shadowing_test() {
  let result = parse_compile_decl("((defn foo () 100) [(foo) (macrolet ((foo () 99)) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func run():
    return [foo(), 99, foo()]
"#);
}

#[test]
#[ignore]
pub fn flet_global_macro_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) [(foo) (flet ((foo () 99)) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func _flet_0():
    return 99
static func run():
    return [100, _flet_0(), 100]
"#);
}

#[test]
#[ignore]
#[should_panic]
pub fn closure_macrolet_test() {
  parse_compile_and_output("(let ((a 1)) (macrolet ((foo () a)) (foo)))");
}

#[test]
#[ignore]
pub fn labels_global_macro_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) [(foo) (labels ((foo () (foo))) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
class _Labels_0 extends Reference:
    func _init():
        pass
    func _fn_foo_2():
        return _fn_foo_2()
static func run():
    var _locals_1 = _Labels_0.new()
    return [100, _locals_1._fn_foo_2(), 100]
"#);
}

#[test]
#[ignore]
pub fn gensym_test_1() {
  let result = parse_compile_decl("((defmacro foo (a) (let ((x (gensym))) `(let ((,x ,a)) [,x ,x]))) (foo 10))");
  assert_eq!(result, r#"extends Reference
static func foo(a_0):
    var x_1 = GDLisp.gensym(null)
    return GDLisp.cons(GDLisp.intern("let"), GDLisp.cons(GDLisp.cons(GDLisp.cons(x_1, GDLisp.cons(a_0, null)), null), GDLisp.cons([x_1, x_1], null)))
static func run():
    var _G_0_2 = 10
    return [_G_0_2, _G_0_2]
"#);
}

#[test]
#[ignore]
pub fn gensym_test_2() {
  let result = parse_compile_decl("((defmacro foo (a) (let ((x (gensym))) `(let ((,x ,a)) [,x ,x]))) [(foo 10) '_G_0])");
  assert_eq!(result, r#"extends Reference
static func foo(a_0):
    var x_1 = GDLisp.gensym(null)
    return GDLisp.cons(GDLisp.intern("let"), GDLisp.cons(GDLisp.cons(GDLisp.cons(x_1, GDLisp.cons(a_0, null)), null), GDLisp.cons([x_1, x_1], null)))
static func run():
    var _G_1_2 = 10
    return [[_G_1_2, _G_1_2], GDLisp.intern("_G_0")]
"#);
}

#[test]
#[ignore]
pub fn macro_inner_class_test_1() {
  let result = parse_compile_decl("((defclass Foo (Reference)) (defmacro foo () (Foo:new) 1) (foo))");
  assert_eq!(result, r#"extends Reference
class Foo extends Reference:
    func _init():
        pass
static func foo():
    Foo.new()
    return 1
static func run():
    return 1
"#);
}

#[test]
#[ignore]
pub fn macro_inner_class_test_2() {
  let result = parse_compile_decl("((defclass Foo (Reference) (defn g () (f))) (defn f () 9) (defmacro foo () ((Foo:new):g)) (foo))");
  assert_eq!(result, r#"extends Reference
class Foo extends Reference:
    func _init():
        pass
    func g():
        return __gdlisp_outer_class_0.f()
    var __gdlisp_outer_class_0 = load("res://TEST.gd")
static func f():
    return 9
static func foo():
    return Foo.new().g()
static func run():
    return 9
"#);
}

#[test]
#[ignore]
pub fn macro_inner_class_test_3() {
  let result = parse_compile_decl("((defclass Foo (Reference) (defn g () static (f))) (defn f () 9) (defmacro foo () (Foo:g)) (foo))");
  assert_eq!(result, r#"extends Reference
class Foo extends Reference:
    func _init():
        pass
    static func g():
        return load("res://TEST.gd").f()
static func f():
    return 9
static func foo():
    return Foo.g()
static func run():
    return 9
"#);
}

#[test]
#[ignore]
pub fn macro_inner_class_test_4() {
  // Can we do it with inheritance in the way?
  let result = parse_compile_decl(r#"
    ((defn add-one (x) (+ x 1))
     (defclass Foo (Reference)
       (defn f () (add-one 10)))
     (defclass Bar (Foo)
       (defn g () (add-one 5)))
     (defmacro foo ()
       (let ((x (Bar:new)))
         (+ (x:f) (x:g))))
     (foo))
"#);
  assert_eq!(result, r#"extends Reference
static func add_one(x_0):
    return x_0 + 1
class Foo extends Reference:
    func _init():
        pass
    func f():
        return __gdlisp_outer_class_1.add_one(10)
    var __gdlisp_outer_class_1 = load("res://TEST.gd")
class Bar extends Foo:
    func _init():
        pass
    func g():
        return __gdlisp_outer_class_2.add_one(5)
    var __gdlisp_outer_class_2 = load("res://TEST.gd")
static func foo():
    var x_3 = Bar.new()
    return x_3.f() + x_3.g()
static func run():
    return 17
"#);
}
