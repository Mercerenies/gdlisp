
use gdlisp::compile::error::{Error as GDError, ErrorF as GDErrorF};
use gdlisp::pipeline::error::{Error as PError};
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn vector_test() {
  assert_eq!(parse_compile_and_output("(vector 9 10)"), "return Vector2(9, 10)\n");
  assert_eq!(parse_compile_and_output("(vector 9 10 11)"), "return Vector3(9, 10, 11)\n");
}

#[test]
pub fn vector_syntax_test() {
  assert_eq!(parse_compile_and_output("V{9 10}"), "return Vector2(9, 10)\n");
  assert_eq!(parse_compile_and_output("V{9 10 11}"), "return Vector3(9, 10, 11)\n");
}

#[test]
pub fn yield_test() {
  assert_eq!(parse_compile_and_output("(yield)"), "return yield()\n");
  assert_eq!(parse_compile_and_output("(yield 1 2)"), "return yield(1, 2)\n");
}

#[test]
pub fn attribute_test() {
  assert_eq!(parse_compile_and_output("(let ((foo 1)) foo:bar)"), "var foo_0 = 1\nreturn foo_0.bar\n");
  assert_eq!(parse_compile_and_output("(let ((foo 1)) foo:bar 2)"), "var foo_0 = 1\nreturn 2\n");
}

#[test]
pub fn method_test() {
  assert_eq!(parse_compile_and_output("(let ((foo 1)) (foo:bar))"), "var foo_0 = 1\nreturn foo_0.bar()\n");
  assert_eq!(parse_compile_and_output("(let ((foo 1)) (foo:bar 100) 2)"), "var foo_0 = 1\nfoo_0.bar(100)\nreturn 2\n");
}

#[test]
pub fn simple_builtin_test() {
  assert_eq!(parse_compile_and_output("(cons 1 2)"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("(cons 1 (cons 2 3))"), "return GDLisp.cons(1, GDLisp.cons(2, 3))\n");
  assert_eq!(parse_compile_and_output("(intern 10)"), "return GDLisp.intern(10)\n");
}

#[test]
pub fn known_gdscript_classes_test_1() {
  assert_eq!(parse_compile_and_output("[Sprite Node Node2D GDScript Object]"),
             "return [Sprite, Node, Node2D, GDScript, GDLisp._Object]\n");
}

#[test]
pub fn known_gdscript_classes_test_2() {
  // Note: They all get checked, but all except the last is elided by the statefulness rules.
  assert_eq!(parse_compile_and_output("(progn Sprite Node Node2D GDScript Object)"), "return GDLisp._Object\n");
}

#[test]
pub fn unknown_gdscript_classes_test() {
  assert_eq!(
    parse_compile_and_output_err("(progn NotARealClass Node2D GDScript Object)"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("NotARealClass")), SourceOffset(7)))),
  );
}

#[test]
pub fn return_test() {
  assert_eq!(parse_compile_and_output("(progn (if 1 (return 2)) 3)"),
             "if 1:\n    return 2\nelse:\n    if true:\n        pass\n    else:\n        pass\nreturn 3\n");
}

#[test]
pub fn yield_running_test() {
  let result = parse_and_run(r#"
    ((defn foo ()
       (print 2)
       (yield)
       (print 4)
       6)
     (print 1)
     (let ((x (foo)))
       (print 3)
       (let ((final (x:resume)))
         (print 5)
         (print final))))
  "#);
  assert_eq!(result, "\n1\n2\n3\n4\n5\n6\n");
}

#[test]
pub fn yield_star_running_test() {
  let result = parse_and_run(r#"
    ((defn foo ()
       (print 2)
       (yield)
       (print 4)
       6)
     (defn bar ()
       (yield* (foo)))
     (print 1)
     (let ((x (bar)))
       (print 3)
       (let ((final (x:resume)))
         (print 5)
         (print final))))
  "#);
  assert_eq!(result, "\n1\n2\n3\n4\n5\n6\n");
}

#[test]
pub fn custom_call_magic_test() {
  assert_eq!(parse_compile_decl("((defn foo (x y) (sys/call-magic ADDITION) 9) (foo 10 20))"),
             r#"extends Reference
static func foo(x_0, y_1):
    return 9
static func run():
    return 10 + 20
"#);
}

#[test]
pub fn custom_call_magic_test_failed() {
  assert_eq!(
    parse_compile_decl_err("((defn foo (x y) (sys/call-magic THIS-MAGIC-DOES-NOT-EXIST) 9))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchMagic(String::from("THIS-MAGIC-DOES-NOT-EXIST")), SourceOffset(2)))),
  );
}

#[test]
pub fn split_call_test() {
  assert_eq!(parse_compile_and_output("(car (car (car (sys/split (car 0)))))"),
             r#"var _split_0 = GDLisp.car(0)
return GDLisp.car(GDLisp.car(GDLisp.car(_split_0)))
"#);
}

// TODO Test gensym at runtime once we can pretty-print symbols
