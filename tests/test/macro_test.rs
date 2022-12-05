
extern crate gdlisp;

use gdlisp::ir::modifier::{ParseError as ModifierParseError, ParseErrorF as ModifierParseErrorF};
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::compile::args::Expecting;
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn simple_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) x))"), r#"extends Reference


static func foo(x):
    return x
"#)
}

#[test]
pub fn constant_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo () 10) (defn bar () (foo)))"), r#"extends Reference


static func foo():
    return 10


static func bar():
    return 10
"#);
}

#[test]
pub fn symbol_macro_test() {
  assert_eq!(parse_compile_decl("((define-symbol-macro foo 10) (defn bar () foo))"), r#"extends Reference


static func __gdlisp_SymbolMacroFunction_foo():
    return 10


static func bar():
    return 10
"#);
}

#[test]
pub fn builtin_symbol_macro_test() {
  assert_eq!(parse_compile_decl("((defn run () [PI SPKEY (let ((PI 1)) PI)]))"), r#"extends Reference


static func run():
    var _PI = 1
    return [PI, SPKEY, _PI]
"#);
}

#[test]
pub fn arithmetic_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 100)) (defn run () (foo 9)))"), r#"extends Reference


static func foo(x):
    return x + 100


static func run():
    return 109
"#);
}

#[test]
pub fn quote_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro my-quote (x) (cons 'quote (cons x ()))) (defn run () (my-quote abc)))"), r#"extends Reference


static func my_quote(x):
    return GDLisp.cons(GDLisp.intern("quote"), GDLisp.cons(x, null))


static func run():
    return GDLisp.intern("abc")
"#);
}

#[test]
pub fn macro_in_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar () (foo 0)) (defn run () (bar)))"), r#"extends Reference


static func foo(x):
    return x + 1


static func bar():
    return 1


static func run():
    return 1
"#);
}

#[test]
pub fn macro_from_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (x) (+ x 1)) (defmacro bar (x) (cons 'foo (cons x ()))) (defn baz () (bar 2)))"), r#"extends Reference


static func foo(x):
    return x + 1


static func bar(x):
    return GDLisp.cons(GDLisp.intern("foo"), GDLisp.cons(x, null))


static func baz():
    return 3
"#);
}

#[test]
pub fn bad_args_macro_test() {
  assert_eq!(
    parse_compile_decl_err("((defmacro foo (x) (+ x 1)) (defmacro bar (x) (cons 'foo (cons x ()))) (defn run () (bar)))"),
    Err(PError::from(GDError::new(GDErrorF::WrongNumberArgs(String::from("bar"), Expecting::exactly(1), 0), SourceOffset(84)))),
  );
}

#[test]
pub fn rest_args_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo (&rest x) x) (defn bar () (foo + 1 2)))"), r#"extends Reference


static func foo(x):
    return x


static func bar():
    return 1 + 2
"#);
}

#[test]
pub fn optional_args_macro_test_1() {
  assert_eq!(parse_compile_decl("((defmacro foo (&opt x) x) (defn bar () (foo 1)))"), r#"extends Reference


static func foo(x):
    return x


static func bar():
    return 1
"#);
}

#[test]
pub fn optional_args_macro_test_2() {
  assert_eq!(parse_compile_decl("((defmacro foo (&opt x) x) (defn bar () (foo)))"), r#"extends Reference


static func foo(x):
    return x


static func bar():
    return null
"#);
}

#[test]
pub fn to_decl_macro_test() {
  assert_eq!(parse_compile_decl("((defmacro foo () '(defn bar () ())) (foo))"), r#"extends Reference


static func foo():
    return GDLisp.cons(GDLisp.intern("defn"), GDLisp.cons(GDLisp.intern("bar"), GDLisp.cons(null, GDLisp.cons(null, null))))


static func bar():
    return null
"#);
}

#[test]
pub fn to_progn_macro_test() {
  assert_eq!(parse_and_run("((defmacro foo () '(progn (defmacro bar (x) x) (print (bar 3)))) (foo))"), "\n3\n");
}

#[test]
pub fn symbol_macro_run_test() {
  assert_eq!(parse_and_run("((define-symbol-macro foo (progn 2)) (print foo))"), "\n2\n");
}

#[test]
pub fn symbol_macro_shadowing_test() {
  assert_eq!(parse_and_run("((define-symbol-macro foo (progn 2)) (print foo) (print (let ((foo 3)) foo)) (print foo))"), "\n2\n3\n2\n");
}

#[test]
pub fn symbol_macro_to_macro_test_1() {
  assert_eq!(parse_and_run("((define-symbol-macro foo '(bar)) (defmacro bar () 9) (print foo))"), "\n9\n");
}

#[test]
pub fn symbol_macro_to_macro_test_2() {
  assert_eq!(parse_and_run("((define-symbol-macro foo '(bar)) (defmacro bar () 9) (print (flet ((bar () 10)) foo)))"), "\n10\n");
}

#[test]
pub fn symbol_macro_to_macro_test_3() {
  assert_eq!(parse_and_run("((define-symbol-macro foo '(bar)) (defmacro bar () 9) (print (macrolet ((bar () 10)) foo)))"), "\n10\n");
}

#[test]
pub fn symbol_macro_to_symbol_macro_test_1() {
  assert_eq!(parse_and_run("((define-symbol-macro foo 'bar) (define-symbol-macro bar 9) (print foo))"), "\n9\n");
}

#[test]
pub fn symbol_macro_to_symbol_macro_test_2() {
  assert_eq!(parse_and_run("((define-symbol-macro foo 'bar) (define-symbol-macro bar 9) (print (let ((bar 10)) foo)))"), "\n10\n");
}

#[test]
pub fn symbol_macro_to_symbol_macro_test_3() {
  assert_eq!(parse_and_run("((define-symbol-macro foo 'bar) (define-symbol-macro bar 9) (print (flet ((bar () 10)) foo)))"), "\n9\n");
}

#[test]
pub fn macro_to_symbol_macro_test_1() {
  assert_eq!(parse_and_run("((defmacro foo () 'bar) (define-symbol-macro bar 9) (print (foo)))"), "\n9\n");
}

#[test]
pub fn macro_to_symbol_macro_test_2() {
  assert_eq!(parse_and_run("((defmacro foo () 'bar) (define-symbol-macro bar 9) (print (let ((bar 10)) (foo))))"), "\n10\n");
}

#[test]
pub fn macro_uses_symbol_macro_test() {
  assert_eq!(parse_and_run("((define-symbol-macro bar 9) (defmacro foo () bar) (print (let ((bar 10)) (foo))))"), "\n9\n");
}

#[test]
pub fn symbol_macrolet_out_of_scope_test() {
  assert_eq!(parse_and_run("((defconst foo 3) (defn bar () foo) (symbol-macrolet ((foo 2)) (print foo) (print (bar))))"), "\n2\n3\n");
}

#[test]
pub fn macrolet_basic_test() {
  let result = parse_compile_and_output("(macrolet ((foo () 100)) (foo))");
  assert_eq!(result, "return 100\n");
}

#[test]
pub fn symbol_macrolet_basic_test() {
  let result = parse_compile_and_output("(symbol-macrolet ((foo 100)) foo)");
  assert_eq!(result, "return 100\n");
}

#[test]
pub fn macrolet_shadowing_test() {
  let result = parse_compile_and_output("(macrolet ((foo () 100)) [(foo) (macrolet ((foo () 99)) (foo)) (foo)])");
  assert_eq!(result, "return [100, 99, 100]\n");
}

#[test]
pub fn symbol_macrolet_shadowing_test() {
  let result = parse_compile_and_output("(symbol-macrolet ((foo 100)) [foo (symbol-macrolet ((foo 99)) foo) foo])");
  assert_eq!(result, "return [100, 99, 100]\n");
}

#[test]
pub fn macrolet_global_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) (defn run () [(foo) (macrolet ((foo () 99)) (foo)) (foo)]))");
  assert_eq!(result, r#"extends Reference


static func foo():
    return 100


static func run():
    return [100, 99, 100]
"#);
}

#[test]
pub fn symbol_macrolet_global_shadowing_test() {
  let result = parse_compile_decl("((defconst foo 100) (defn run () [foo (symbol-macrolet ((foo 99)) foo) foo]))");
  assert_eq!(result, r#"extends Reference


const foo = 100


static func run():
    return [foo, 99, foo]
"#);
}

#[test]
pub fn symbol_macro_shared_name_with_function_test() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo 100)
                               (defn foo () 101)
                               (print foo)
                               (print (foo)))"#),
             "\n100\n101\n");
}

#[test]
pub fn symbol_macro_shared_name_with_macro_test() {
  assert_eq!(parse_and_run(r#"((define-symbol-macro foo 100)
                               (defmacro foo () 101)
                               (print foo)
                               (print (foo)))"#),
             "\n100\n101\n");
}

#[test]
pub fn macrolet_global_function_shadowing_test_1() {
  let result = parse_compile_decl("((defn foo () 100) (defn run () [(foo) (macrolet ((foo () 99)) (foo)) (foo)]))");
  assert_eq!(result, r#"extends Reference


static func foo():
    return 100


static func run():
    return [foo(), 99, foo()]
"#);
}

#[test]
pub fn macrolet_global_function_shadowing_test_2() {
  let result = parse_compile_decl("((defn foo () 100) (defn run () [(foo) (macrolet ((foo () (foo))) (foo)) (foo)]))");
  assert_eq!(result, r#"extends Reference


static func foo():
    return 100


static func run():
    return [foo(), 100, foo()]
"#);
}

#[test]
pub fn symbol_macrolet_global_function_shadowing_test_1() {
  let result = parse_compile_decl("((defconst foo 100) (defn run () [foo (symbol-macrolet ((foo 99)) foo) foo]))");
  assert_eq!(result, r#"extends Reference


const foo = 100


static func run():
    return [foo, 99, foo]
"#);
}

#[test]
pub fn symbol_macrolet_global_function_shadowing_test_2() {
  let result = parse_compile_decl("((defconst foo 100) (defn run () [foo (symbol-macrolet ((foo foo)) foo) foo]))");
  assert_eq!(result, r#"extends Reference


const foo = 100


static func run():
    return [foo, 100, foo]
"#);
}

#[test]
pub fn symbol_macrolet_global_function_shadowing_test_3() {
  let result = parse_compile_decl("((define-symbol-macro foo 100) (defn run () [foo (symbol-macrolet ((foo foo)) foo) foo]))");
  assert_eq!(result, r#"extends Reference


static func __gdlisp_SymbolMacroFunction_foo():
    return 100


static func run():
    return [100, 100, 100]
"#);
}

#[test]
pub fn symbol_macrolet_global_function_shadowing_test_4() {
  let result = parse_compile_decl("((define-symbol-macro foo 100) (defn run () [foo (symbol-macrolet ((foo 99)) foo) foo]))");
  assert_eq!(result, r#"extends Reference


static func __gdlisp_SymbolMacroFunction_foo():
    return 100


static func run():
    return [100, 99, 100]
"#);
}

#[test]
pub fn flet_global_macro_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) (defn run () [(foo) (flet ((foo () 99)) (foo)) (foo)]))");
  assert_eq!(result, r#"extends Reference


static func foo():
    return 100


static func _flet():
    return 99


static func run():
    return [100, _flet(), 100]
"#);
}

#[test]
pub fn closure_macrolet_test_1() {
  assert_eq!(
    parse_compile_and_output_err("(let ((a 1)) (macrolet ((foo () a)) (foo)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("a")), SourceOffset(32)))),
  );
}

#[test]
pub fn closure_macrolet_test_2() {
  assert_eq!(
    parse_compile_and_output_err("(flet ((f () 1)) (macrolet ((foo () (f))) (foo)))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("f")), SourceOffset(36)))),
  );
}

#[test]
pub fn labels_global_macro_shadowing_test() {
  let result = parse_compile_decl("((defmacro foo () 100) (defn run () [(foo) (labels ((foo () (foo))) (foo)) (foo)]))");
  assert_eq!(result, r#"extends Reference


static func foo():
    return 100


class _Labels extends Reference:

    func _init():
        pass

    func _fn_foo_0():
        return _fn_foo_0()


static func run():
    var _locals = _Labels.new()
    return [100, _locals._fn_foo_0(), 100]
"#);
}

#[test]
pub fn gensym_test_1() {
  let result = parse_compile_decl("((defmacro foo (a) (let ((x (gensym))) `(let ((,x ,a)) [,x ,x]))) (defn bar () (foo 10)))");
  assert_eq!(result, r#"extends Reference


static func foo(a):
    var x = GDLisp.gensym(null)
    var _quasiquote = null
    var _quasiquote_0 = GDLisp.cons(x, null)
    return GDLisp.cons(GDLisp.intern("let"), GDLisp.cons(GDLisp.cons(GDLisp.cons(x, GDLisp.cons(a, _quasiquote)), null), GDLisp.cons(GDLisp.cons(GDLisp.intern("array"), GDLisp.cons(x, _quasiquote_0)), null)))


static func bar():
    var _G_0 = 10
    return [_G_0, _G_0]
"#);
}

#[test]
pub fn gensym_test_2() {
  let result = parse_compile_decl("((defmacro foo (a) (let ((x (gensym))) `(let ((,x ,a)) [,x ,x]))) (defn bar () [(foo 10) '_G_0]))");
  assert_eq!(result, r#"extends Reference


static func foo(a):
    var x = GDLisp.gensym(null)
    var _quasiquote = null
    var _quasiquote_0 = GDLisp.cons(x, null)
    return GDLisp.cons(GDLisp.intern("let"), GDLisp.cons(GDLisp.cons(GDLisp.cons(x, GDLisp.cons(a, _quasiquote)), null), GDLisp.cons(GDLisp.cons(GDLisp.intern("array"), GDLisp.cons(x, _quasiquote_0)), null)))


static func bar():
    var _G_1 = 10
    return [[_G_1, _G_1], GDLisp.intern("_G_0")]
"#);
}

#[test]
pub fn macro_inner_class_test_1() {
  let result = parse_compile_decl("((defclass Foo (Reference)) (defmacro foo () (Foo:new) 1) (defn run () (foo)))");
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
pub fn macro_inner_class_test_2() {
  let result = parse_compile_decl("((defclass Foo (Reference) (defn g () (f))) (defn f () 9) (defmacro foo () ((Foo:new):g)) (defn run () (foo)))");
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
pub fn macro_inner_class_test_3() {
  let result = parse_compile_decl("((defclass Foo (Reference) (defn g () static (f))) (defn f () 9) (defmacro foo () (Foo:g)) (defn run () (foo)))");
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
     (defn run-test () (foo)))
"#);
  assert_eq!(result, r#"extends Reference


static func add_one(x):
    return x + 1


class Foo extends Reference:

    func _init():
        pass

    func f():
        return __gdlisp_outer_class_0.add_one(10)

    var __gdlisp_outer_class_0 = load("res://TEST.gd")


class Bar extends Foo:

    func _init():
        pass

    func g():
        return __gdlisp_outer_class_1.add_one(5)

    var __gdlisp_outer_class_1 = load("res://TEST.gd")


static func foo():
    var x = Bar.new()
    return x.f() + x.g()


static func run_test():
    return 17
"#);
}

#[test]
pub fn nonsense_modifier_macro_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defmacro foo () public private 1))"#),
    Err(PError::from(ModifierParseError::new(ModifierParseErrorF::UniquenessError(String::from("visibility")), SourceOffset(25)))),
  );
}

#[test]
pub fn macro_in_minimalist_test() {
  assert_eq!(
    parse_compile_decl_err("((sys/nostdlib) (defmacro foo () 10) (foo))"),
    Err(PError::from(GDError::new(GDErrorF::MacroInMinimalistError(String::from("foo")), SourceOffset(37)))),
  );
}

#[test]
pub fn simple_minimalist_test() {
  assert_eq!(parse_compile_decl("((sys/nostdlib))"), r#"extends Reference


func _init():
    pass
"#);
}

#[test]
pub fn quit_macro_test() {
  let result = parse_compile_and_output("(quit)");
  assert_eq!(result, "return GDLisp.get_tree().quit()\n");
}

#[test]
pub fn recursive_macro_test() {
  assert_eq!(
    parse_compile_decl_err("((defmacro foo () (foo)))"),
    Err(PError::from(GDError::new(GDErrorF::MacroBeforeDefinitionError(String::from("foo")), SourceOffset(18)))),
  );
}

#[test]
pub fn recursive_macrolet_test() {
  assert_eq!(
    parse_compile_decl_err("((macrolet ((foo () (foo))) ()))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchFn(String::from("foo")), SourceOffset(20)))),
  );
}

#[test]
pub fn bad_order_macro_test() {
  assert_eq!(
    parse_compile_decl_err("((defn bar () (foo)) (defmacro foo () 1))"),
    Err(PError::from(GDError::new(GDErrorF::MacroBeforeDefinitionError(String::from("foo")), SourceOffset(14)))),
  );
}

#[test]
pub fn bad_preload_in_macro_test() {
  assert_eq!(
    parse_compile_decl_err(r#"((defmacro foo ()
                                  (sys/context-filename "res://not-a-real-file.lisp")))"#),
    Err(PError::from(GDError::new(GDErrorF::ContextualFilenameUnresolved, SourceOffset(52)))),
  );
}
