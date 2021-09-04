
use super::common::*;

#[test]
pub fn quote_test() {
  assert_eq!(parse_compile_and_output("(quote 10)"), "return 10\n");
  assert_eq!(parse_compile_and_output("(quote (1 2))"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("(quote (1 . 2))"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("(quote [1 2])"), "return [1, 2]\n");
}

#[test]
pub fn quote_syntax_test() {
  assert_eq!(parse_compile_and_output("'10"), "return 10\n");
  assert_eq!(parse_compile_and_output("'(1 2)"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("'(1 . 2)"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("'[1 2]"), "return [1, 2]\n");
}

#[test]
pub fn full_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(quasiquote 10)"), "return 10\n");
  assert_eq!(parse_compile_and_output("(quasiquote (1 2))"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("(quasiquote (1 . 2))"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("(quasiquote [1 2])"), "return [1, 2]\n");
}

#[test]
pub fn full_quasiquote_syntax_test() {
  assert_eq!(parse_compile_and_output("`10"), "return 10\n");
  assert_eq!(parse_compile_and_output("`(1 2)"), "return GDLisp.cons(1, GDLisp.cons(2, null))\n");
  assert_eq!(parse_compile_and_output("`(1 . 2)"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("`[1 2]"), "return [1, 2]\n");
}

#[test]
pub fn partial_quasiquote_test_1() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `,a)"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(a . ,a))"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a_0)\n");
}

#[test]
pub fn partial_quasiquote_test_2() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote ,a))"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (a . ,a)))"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a_0)\n");
}

#[test]
pub fn partial_quasiquote_test_3() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (unquote a)))"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) (quasiquote (a . (unquote a))))"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a_0)\n");
}

#[test]
pub fn partial_quasiquote_test_4() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(unquote a))"), "var a_0 = 1\nreturn a_0\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `(a . (unquote a)))"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"a\"), a_0)\n");
}

#[test]
pub fn array_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `[a ,a a])"), "var a_0 = 1\nreturn [GDLisp.intern(\"a\"), a_0, GDLisp.intern(\"a\")]\n");
}

#[test]
pub fn vector_quote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) 'V{a a})"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(GDLisp.intern(\"a\"), null)))\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) 'V{a a a})"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(GDLisp.intern(\"a\"), null))))\n");
}

#[test]
pub fn vector_quasiquote_test() {
  assert_eq!(parse_compile_and_output("(let ((a 1)) `V{a ,a})"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(a_0, null)))\n");
  assert_eq!(parse_compile_and_output("(let ((a 1)) `V{a ,a a})"), "var a_0 = 1\nreturn GDLisp.cons(GDLisp.intern(\"vector\"), GDLisp.cons(GDLisp.intern(\"a\"), GDLisp.cons(a_0, GDLisp.cons(GDLisp.intern(\"a\"), null))))\n");
}

#[test]
pub fn quasiquote_unquote_spliced_list_test() {
  assert_eq!(parse_compile_and_output("(let ((a [2 3])) `(1 ,.a 4))"),
             "var a_0 = [2, 3]\nreturn GDLisp.cons(1, GDLisp.append(GDLisp.Cons.new(GDLisp.sys_u002Fqq_smart_list(a_0), GDLisp.Cons.new(GDLisp.cons(4, null), null))))\n");
}

#[test]
pub fn quasiquote_nested_test() {
  assert_eq!(parse_compile_and_output("``(,a)"),
             "var _split_0 = null\nreturn GDLisp.cons(GDLisp.intern(\"quasiquote\"), GDLisp.cons(GDLisp.cons(GDLisp.cons(GDLisp.intern(\"unquote\"), GDLisp.cons(GDLisp.intern(\"a\"), _split_0)), null), null))\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_list_test_runner_1() {
  assert_eq!(parse_and_run("((let ((a [2 3])) (print (list->array `(1 ,.a 4)))))"), "\n[1, 2, 3, 4]\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_list_test_runner_2() {
  assert_eq!(parse_and_run("((let ((a '(2 3))) (print (list->array `(1 ,.a 4)))))"), "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn quasiquote_unquote_spliced_array_test() {
  assert_eq!(parse_compile_and_output("(let ((a [3 4])) `[1 2 ,.a 5 6])"),
             "var a_0 = [3, 4]\nreturn [1, 2] + GDLisp.sys_u002Fqq_smart_array(a_0) + [5, 6]\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_array_test_runner_1() {
  assert_eq!(parse_and_run("((let ((a [3 4])) (print `[1 2 ,.a 5 6])))"), "\n[1, 2, 3, 4, 5, 6]\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_array_test_runner_2() {
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print `[1 2 ,.a 5 6])))"), "\n[1, 2, 3, 4, 5, 6]\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_array_test_runner_3() {
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print `[1 2 ,.a])))"), "\n[1, 2, 3, 4]\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_array_test_runner_4() {
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print `[1 2 ,.a ,.a])))"), "\n[1, 2, 3, 4, 3, 4]\n");
}

#[test]
#[ignore]
pub fn quasiquote_unquote_spliced_array_test_runner_5() {
  assert_eq!(parse_and_run("((let ((a '(3 4))) (print `[,.a 10 ,.a])))"), "\n[3, 4, 10, 3, 4]\n");
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
             "var _split_0 = null\nreturn GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, GDLisp.cons(4, GDLisp.cons(5, _split_0)))))\n");
  assert_eq!(parse_compile_and_output("`(1 2 3 4 5 6)"),
             "var _split_0 = GDLisp.cons(6, null)\nreturn GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, GDLisp.cons(4, GDLisp.cons(5, _split_0)))))\n");
}
