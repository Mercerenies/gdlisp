
pub mod common;

use common::parse_compile_and_output;

#[test]
pub fn quote_test() {
  assert_eq!(parse_compile_and_output("(quote 10)"), "return 10\n");
  assert_eq!(parse_compile_and_output("(quote (1 2))"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, GDLisp.Nil))\n");
  assert_eq!(parse_compile_and_output("(quote (1 . 2))"), "return GDLisp.Cons.new(1, 2)\n");
  assert_eq!(parse_compile_and_output("(quote [1 2])"), "return [1, 2]\n");
}

#[test]
pub fn quote_syntax_test() {
  assert_eq!(parse_compile_and_output("'10"), "return 10\n");
  assert_eq!(parse_compile_and_output("'(1 2)"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, GDLisp.Nil))\n");
  assert_eq!(parse_compile_and_output("'(1 . 2)"), "return GDLisp.Cons.new(1, 2)\n");
  assert_eq!(parse_compile_and_output("'[1 2]"), "return [1, 2]\n");
}

#[test]
pub fn full_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(quasiquote 10)"), "return 10\n");
  assert_eq!(parse_compile_and_output("(quasiquote (1 2))"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, GDLisp.Nil))\n");
  assert_eq!(parse_compile_and_output("(quasiquote (1 . 2))"), "return GDLisp.Cons.new(1, 2)\n");
  assert_eq!(parse_compile_and_output("(quasiquote [1 2])"), "return [1, 2]\n");
}

#[test]
pub fn full_quasiquote_syntax_test() {
  assert_eq!(parse_compile_and_output("`10"), "return 10\n");
  assert_eq!(parse_compile_and_output("`(1 2)"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, GDLisp.Nil))\n");
  assert_eq!(parse_compile_and_output("`(1 . 2)"), "return GDLisp.Cons.new(1, 2)\n");
  assert_eq!(parse_compile_and_output("`[1 2]"), "return [1, 2]\n");
}

#[test]
pub fn partial_quasiquote_test_1() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `,a)"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(a . ,a))"), "var a_0 = 1\nreturn GDLisp.Cons.new(GDLisp.Symbol.new(\"a\"), a_0)\n");
}

#[test]
pub fn partial_quasiquote_test_2() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote ,a))"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (a . ,a)))"), "var a_0 = 1\nreturn GDLisp.Cons.new(GDLisp.Symbol.new(\"a\"), a_0)\n");
}

#[test]
pub fn partial_quasiquote_test_3() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (unquote a)))"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (a . (unquote a))))"), "var a_0 = 1\nreturn GDLisp.Cons.new(GDLisp.Symbol.new(\"a\"), a_0)\n");
}

#[test]
pub fn partial_quasiquote_test_4() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(unquote a))"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(a . (unquote a)))"), "var a_0 = 1\nreturn GDLisp.Cons.new(GDLisp.Symbol.new(\"a\"), a_0)\n");
}

#[test]
pub fn array_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `[a ,a a])"), "var a_0 = 1\nreturn [GDLisp.Symbol.new(\"a\"), a_0, GDLisp.Symbol.new(\"a\")]\n");
}
