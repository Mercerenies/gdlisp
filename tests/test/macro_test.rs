
extern crate gdlisp;

use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::compile::args::Expecting;
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

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
pub fn symbol_macro_test() {
  assert_eq!(parse_compile_decl("((define-symbol-macro foo 10) foo)"), "extends Reference\nstatic func foo():\n    return 10\nstatic func run():\n    return 10\n");
}

#[test]
#[ignore]
pub fn builtin_symbol_macro_test() {
  assert_eq!(parse_compile_decl("([PI SPKEY (let ((PI 1)) PI)])"), "extends Reference\nstatic func run():\n    var _PI_0 = 1\n    return [PI, SPKEY, _PI_0]\n");
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
#[ignore]
pub fn bad_args_macro_test() {
  assert_eq!(
    parse_compile_decl_err("((defmacro foo (x) (+ x 1)) (defmacro bar (x) (cons 'foo (cons x ()))) (bar))"),
    Err(PError::from(GDError::new(GDErrorF::WrongNumberArgs(String::from("bar"), Expecting::exactly(1), 0), SourceOffset(71)))),
  );
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
pub fn symbol_macro_run_test() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo (progn 2)) (print foo))"#), "\n2\n");
}

#[test]
#[ignore]
pub fn symbol_macro_shadowing_test() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo (progn 2)) (print foo) (print (let ((foo 3)) foo)) (print foo))"#), "\n2\n3\n2\n");
}

#[test]
#[ignore]
pub fn symbol_macro_to_macro_test_1() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo '(bar)) (defmacro bar () 9) (print foo))"#), "\n9\n");
}

#[test]
#[ignore]
pub fn symbol_macro_to_macro_test_2() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo '(bar)) (defmacro bar () 9) (print (flet ((bar () 10)) foo)))"#), "\n10\n");
}

#[test]
#[ignore]
pub fn symbol_macro_to_macro_test_3() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo '(bar)) (defmacro bar () 9) (print (macrolet ((bar () 10)) foo)))"#), "\n10\n");
}

#[test]
#[ignore]
pub fn symbol_macro_to_symbol_macro_test_1() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo 'bar) (define-symbol-macro bar 9) (print foo))"#), "\n9\n");
}

#[test]
#[ignore]
pub fn symbol_macro_to_symbol_macro_test_2() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo 'bar) (define-symbol-macro bar 9) (print (let ((bar 10)) foo)))"#), "\n10\n");
}

#[test]
#[ignore]
pub fn symbol_macro_to_symbol_macro_test_3() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo 'bar) (define-symbol-macro bar 9) (print (flet ((bar () 10)) foo)))"#), "\n9\n");
}

#[test]
#[ignore]
pub fn macro_to_symbol_macro_test_1() {
  assert_eq!(parse_and_run(r#"((defmacro foo () 'bar) (define-symbol-macro bar 9) (print (foo)))"#), "\n9\n");
}

#[test]
#[ignore]
pub fn macro_to_symbol_macro_test_2() {
  assert_eq!(parse_and_run(r#"((defmacro foo () 'bar) (define-symbol-macro bar 9) (print (let ((bar 10)) (foo))))"#), "\n10\n");
}

#[test]
#[ignore]
pub fn macro_uses_symbol_macro_test() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro bar 9) (defmacro foo () bar) (print (let ((bar 10)) (foo))))"#), "\n9\n");
}

#[test]
#[ignore]
pub fn symbol_macrolet_out_of_scope_test() {
  assert_eq!(parse_and_run(r#"((defconst foo 3) (defn bar () foo) (symbol-macrolet ((foo 2)) (print foo) (print (bar))))"#), "\n2\n3\n");
}

#[test]
#[ignore]
pub fn macrolet_basic_test() {
  let result = parse_compile_and_output("(macrolet ((foo () 100)) (foo))");
  assert_eq!(result, "return 100\n");
}

#[test]
#[ignore]
pub fn symbol_macrolet_basic_test() {
  let result = parse_compile_and_output("(symbol-macrolet ((foo 100)) foo)");
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
pub fn symbol_macrolet_shadowing_test() {
  let result = parse_compile_and_output("(symbol-macrolet ((foo 100)) [foo (symbol-macrolet ((foo 99)) foo) foo])");
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
pub fn symbol_macrolet_global_shadowing_test() {
  let result = parse_compile_decl("((defconst foo 100) [foo (symbol-macrolet ((foo 99)) foo) foo])");
  assert_eq!(result, r#"extends Reference
const foo = 100
static func run():
    return [foo, 99, foo]
"#);
}

#[test]
#[ignore]
pub fn macrolet_global_function_shadowing_test_1() {
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
pub fn macrolet_global_function_shadowing_test_2() {
  let result = parse_compile_decl("((defn foo () 100) [(foo) (macrolet ((foo () (foo))) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func run():
    return [foo(), 100, foo()]
"#);
}

#[test]
#[ignore]
pub fn symbol_macrolet_global_function_shadowing_test_1() {
  let result = parse_compile_decl("((defconst foo 100) [foo (symbol-macrolet ((foo 99)) foo) foo])");
  assert_eq!(result, r#"extends Reference
const foo = 100
static func run():
    return [foo, 99, foo]
"#);
}

#[test]
#[ignore]
pub fn symbol_macrolet_global_function_shadowing_test_2() {
  let result = parse_compile_decl("((defconst foo 100) [foo (symbol-macrolet ((foo foo)) foo) foo])");
  assert_eq!(result, r#"extends Reference
const foo = 100
static func run():
    return [foo, 100, foo]
"#);
}

#[test]
#[ignore]
pub fn symbol_macrolet_global_function_shadowing_test_3() {
  let result = parse_compile_decl("((define-symbol-macro foo 100) [foo (symbol-macrolet ((foo foo)) foo) foo])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func run():
    return [100, 100, 100]
"#);
}

#[test]
#[ignore]
pub fn symbol_macrolet_global_function_shadowing_test_4() {
  let result = parse_compile_decl("((define-symbol-macro foo 100) [foo (symbol-macrolet ((foo 99)) foo) foo])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
static func run():
    return [100, 99, 100]
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
pub fn closure_macrolet_test_1() {
  // TODO Why is this reporting missing variable at macrolet, not at
  // the variable? Also applies to closure_macrolet_test_2 and
  // recursive_macrolet_test.
  assert_eq!(
    parse_compile_and_output_err("(let ((a 1)) (macrolet ((foo () a)) (foo)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("a")), SourceOffset(13)))),
  );
}

#[test]
#[ignore]
pub fn closure_macrolet_test_2() {
  assert_eq!(
    parse_compile_and_output_err("(flet ((f () 1)) (macrolet ((foo () (f))) (foo)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("f")), SourceOffset(17)))),
  );
}

#[test]
#[ignore]
pub fn labels_global_macro_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) [(foo) (labels ((foo () (foo))) (foo)) (foo)])");
  assert_eq!(result, r#"extends Reference
static func foo():
    return 100
class _Labels_2 extends Reference:
    func _init():
        pass
    func _fn_foo_1():
        return _fn_foo_1()
static func run():
    var _locals_0 = _Labels_2.new()
    return [100, _locals_0._fn_foo_1(), 100]
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

#[test]
#[ignore]
pub fn nonsense_modifier_macro_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defmacro foo () public private 1))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(25)))),
  );
}

#[test]
#[ignore]
pub fn macro_in_minimalist_test() {
  assert_eq!(
    parse_compile_decl_err("((sys/nostdlib) (defmacro foo () 10) (foo))"),
    Err(PError::from(GDError::new(GDErrorF::MacroInMinimalistError(String::from("foo")), SourceOffset(37)))),
  );
}

#[test]
pub fn simple_minimalist_test() {
  assert_eq!(parse_compile_decl("((sys/nostdlib))"), r#"extends Reference
static func run():
    return null
"#);
}

#[test]
#[ignore]
pub fn recursive_macro_test() {
  assert_eq!(
    parse_compile_decl_err("((defmacro foo () (foo)))"),
    Err(PError::from(GDError::new(GDErrorF::MacroBeforeDefinitionError(String::from("foo")), SourceOffset(18)))),
  );
}

#[test]
#[ignore]
pub fn recursive_macrolet_test() {
  assert_eq!(
    parse_compile_decl_err("((macrolet ((foo () (foo))) ()))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("foo")), SourceOffset(1)))),
  );
}

#[test]
#[ignore]
pub fn bad_order_macro_test() {
  assert_eq!(
    parse_compile_decl_err("((defn bar () (foo)) (defmacro foo () 1))"),
    Err(PError::from(GDError::new(GDErrorF::MacroBeforeDefinitionError(String::from("foo")), SourceOffset(14)))),
  );
}
