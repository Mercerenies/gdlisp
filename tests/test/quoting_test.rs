
use super::common::*;

#[test]
pub fn quote_test() {
  assert_eq!(parse_compile_and_output("(quote 10)"), "return 10\n");
  assert_eq!(parse_compile_and_output("(quote (1 2))"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("(quote (1 . 2))"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("(quote [1 2])"), "return GDLisp.cons(GDLisp.intern(\"array\"), GDLisp.cons(1, GDLisp.cons(2, null)))\n");
}

#[test]
pub fn quote_syntax_test() {
  assert_eq!(parse_compile_and_output("'10"), "return 10\n");
  assert_eq!(parse_compile_and_output("'(1 2)"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("'(1 . 2)"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("'[1 2]"), "return GDLisp.cons(GDLisp.intern(\"array\"), GDLisp.cons(1, GDLisp.cons(2, null)))\n");
}

#[test]
pub fn full_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(quasiquote 10)"), "return 10\n");
  assert_eq!(parse_compile_and_output("(quasiquote (1 2))"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("(quasiquote (1 . 2))"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("(quasiquote [1 2])"), "return GDLisp.cons(GDLisp.intern(\"array\"), GDLisp.cons(1, GDLisp.cons(2, null)))\n");
}

#[test]
pub fn full_quasiquote_syntax_test() {
  assert_eq!(parse_compile_and_output("`10"), "return 10\n");
  assert_eq!(parse_compile_and_output("`(1 2)"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("`(1 . 2)"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("`[1 2]"), "return GDLisp.cons(GDLisp.intern(\"array\"), GDLisp.cons(1, GDLisp.cons(2, null)))\n");
}

#[test]
pub fn partial_quasiquote_test_1() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `,a)"), "var a = 1\nreturn a\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(a . ,a))"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a)\n");
}

#[test]
pub fn partial_quasiquote_test_2() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote ,a))"), "var a = 1\nreturn a\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (a . ,a)))"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a)\n");
}

#[test]
pub fn partial_quasiquote_test_3() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (unquote a)))"), "var a = 1\nreturn a\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (a . (unquote a))))"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a)\n");
}

#[test]
pub fn partial_quasiquote_test_4() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(unquote a))"), "var a = 1\nreturn a\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(a . (unquote a)))"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a)\n");
}

#[test]
pub fn array_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `[a ,a a])"), r#"var a = 1
return GDLisp.cons(GDLisp.intern("array"), GDLisp.cons(GDLisp.intern("a"), GDLisp.cons(a, GDLisp.cons(GDLisp.intern("a"), null))))
"#);
}

#[test]
pub fn vector_quote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) 'V{a a})"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(GDLisp.intern(\"a\"), null)))\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) 'V{a a a})"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(GDLisp.intern(\"a\"), null))))\n");
}

#[test]
pub fn vector_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `V{a ,a})"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(a, null)))\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `V{a ,a a})"), "var a = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(a, GDLisp.cons(GDLisp.intern(\"a\"), null))))\n");
}

#[test]
pub fn quasiquote_unquote_spliced_list_test() {
  assert_eq!(parse_compile_and_output("(let ((a [2 3])) `(1 ,.a 4))"),
             "var a = [2, 3]\nreturn GDLisp.cons(1, GDLisp.append(GDLisp.cons(GDLisp.sys_DIV_qq_smart_list(a), GDLisp.cons(GDLisp.cons(4, null), null))))\n");
}

#[test]
pub fn quasiquote_nested_test() {
  assert_eq!(parse_compile_and_output("``(,a)"),
             "var _quasiquote = null\nreturn GDLisp.cons(GDLisp.intern(\"quasiquote\"), GDLisp.cons(GDLisp.cons(GDLisp.cons(GDLisp.intern(\"unquote\"), GDLisp.cons(GDLisp.intern(\"a\"), _quasiquote)), null), null))\n");
}

#[test]
pub fn quasiquote_unquote_spliced_list_test_runner_1() {
  assert_eq!(parse_and_run("((let ((a [2 3])) (print (list->array `(1 ,.a 4)))))"), "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_list_test_runner_2() {
  assert_eq!(parse_and_run("((let ((a '(2 3))) (print (list->array `(1 ,.a 4)))))"), "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test() {
  assert_eq!(parse_compile_and_output("(let ((a [2 3])) `[1 ,.a 4])"),
             "var a = [2, 3]\nreturn GDLisp.cons(GDLisp.intern(\"array\"), GDLisp.cons(1, GDLisp.append(GDLisp.cons(GDLisp.sys_DIV_qq_smart_list(a), GDLisp.cons(GDLisp.cons(4, null), null)))))\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test_runner_1() {
  // (list->array ...:cdr) removes the 'array term from the head and
  // then prints as a Godot array.
  assert_eq!(parse_and_run("((let ((a [3 4])) (print (list->array (quasiquote [1 2 ,.a 5 6]):cdr))))"), "\n[1, 2, 3, 4, 5, 6]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test_runner_2() {
  // (list->array ...:cdr) removes the 'array term from the head and
  // then prints as a Godot array.
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print (list->array (quasiquote [1 2 ,.a 5 6]):cdr))))"), "\n[1, 2, 3, 4, 5, 6]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test_runner_3() {
  // (list->array ...:cdr) removes the 'array term from the head and
  // then prints as a Godot array.
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print (list->array (quasiquote [1 2 ,.a]):cdr))))"), "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test_runner_4() {
  // (list->array ...:cdr) removes the 'array term from the head and
  // then prints as a Godot array.
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print (list->array (quasiquote [1 2 ,.a ,.a]):cdr))))"), "\n[1, 2, 3, 4, 3, 4]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test_runner_5() {
  // (list->array ...:cdr) removes the 'array term from the head and
  // then prints as a Godot array.
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print (list->array (quasiquote [,.a 10 ,.a]):cdr))))"), "\n[3, 4, 10, 3, 4]\n");
}

#[test]
pub fn quasiquote_nesting_test() {
  assert_eq!(parse_compile_and_output("`(1)"),
             "return GDLisp.cons(1, null)\n");
  assert_eq!(parse_compile_and_output("`(1 2)"),
             "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("`(1 2 3)"),
             "return GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, null)))\n");
  assert_eq!(parse_compile_and_output("`(1 2 3 4)"),
             "return GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, GDLisp.cons(4, null))))\n");
  assert_eq!(parse_compile_and_output("`(1 2 3 4 5)"),
             "var _quasiquote = null\nreturn GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, GDLisp.cons(4, GDLisp.cons(5, _quasiquote)))))\n");
  assert_eq!(parse_compile_and_output("`(1 2 3 4 5 6)"),
             "var _quasiquote = GDLisp.cons(6, null)\nreturn GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, GDLisp.cons(4, GDLisp.cons(5, _quasiquote)))))\n");
}
