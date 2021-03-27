
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
  assert_eq!(parse_compile_decl("((defmacro my-quote (x) (cons 'quote (cons x ()))) (my-quote abc))"), "extends Reference\nstatic func my_quote(x_0):\n    return GDLisp.Cons.new(GDLisp.Symbol.new(\"quote\"), GDLisp.Cons.new(x_0, null))\nstatic func run():\n    return GDLisp.Symbol.new(\"abc\")\n");
}

#[test]
#[ignore]
pub fn macro_in_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar () (foo 0)) (bar))"), "extends Reference\nstatic func foo(x_0):\n    return x_0 + 1\nstatic func bar():\n    return 1\nstatic func run():\n    return 1\n");
}

#[test]
#[ignore]
pub fn macro_from_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar (x) (cons 'foo (cons x ()))) (bar 2))"), "extends Reference\nstatic func foo(x_0):\n    return x_0 + 1\nstatic func bar(x_1):\n    return GDLisp.Cons.new(GDLisp.Symbol.new(\"foo\"), GDLisp.Cons.new(x_1, null))\nstatic func run():\n    return 3\n");
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
  assert_eq!(parse_compile_decl("((defmacro foo () '(defn bar () ())) (foo))"), "extends Reference\nstatic func foo():\n    return GDLisp.Cons.new(GDLisp.Symbol.new(\"defn\"), GDLisp.Cons.new(GDLisp.Symbol.new(\"bar\"), GDLisp.Cons.new(null, GDLisp.Cons.new(null, null))))\nstatic func bar():\n    return null\nstatic func run():\n    return null\n");
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

/*
#[test]
#[ignore]
pub fn labels_global_macro_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) [(foo) (labels ((foo () (foo))) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func _flet_0():
    return 99
static func run():
    return [100, _flet_0(), 100]
"#);
}
*/
