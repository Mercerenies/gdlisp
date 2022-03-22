
use super::common::*;

#[test]
fn addition_test_1() {
  assert_eq!(parse_and_run("((print (+)))"), "\n0\n");
}

#[test]
fn addition_test_2() {
  assert_eq!(parse_and_run("((print (+ 1)))"), "\n1\n");
}

#[test]
fn addition_test_3() {
  assert_eq!(parse_and_run("((print (+ 1 5)))"), "\n6\n");
}

#[test]
fn addition_test_4() {
  assert_eq!(parse_and_run("((print (+ 1 5 2)))"), "\n8\n");
}

#[test]
fn addition_test_5() {
  assert_eq!(parse_and_run("((print (+ 1 5 2 -4)))"), "\n4\n");
}

#[test]
fn addition_test_6() {
  assert_eq!(parse_and_run("((print (+ (vector 1 1) (vector 2 3))))"), "\n(3, 4)\n");
}

#[test]
fn addition_test_7() {
  assert_eq!(parse_and_run("((print (+ (vector 1 1))))"), "\n(1, 1)\n");
}

#[test]
fn addition_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function +) 1 5 2 -4)))"), "\n4\n");
}

#[test]
fn addition_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function +))))"), "\n0\n");
}

#[test]
fn addition_test_indirect_3() {
  assert_eq!(parse_and_run("((print (funcall (function +) 3)))"), "\n3\n");
}

#[test]
fn addition_test_indirect_4() {
  assert_eq!(parse_and_run("((print (apply (function +) 1 2 '(3 4))))"), "\n10\n");
}

#[test]
fn multiplication_test_1() {
  assert_eq!(parse_and_run("((print (*)))"), "\n1\n");
}

#[test]
fn multiplication_test_2() {
  assert_eq!(parse_and_run("((print (* 4)))"), "\n4\n");
}

#[test]
fn multiplication_test_3() {
  assert_eq!(parse_and_run("((print (* 4 3)))"), "\n12\n");
}

#[test]
fn multiplication_test_4() {
  assert_eq!(parse_and_run("((print (* 4 3 2)))"), "\n24\n");
}

#[test]
fn multiplication_test_5() {
  assert_eq!(parse_and_run("((print (* 4 (vector 1 2) 2 (vector 2 3))))"), "\n(16, 48)\n");
}

#[test]
fn multiplication_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function *) 4 3 2)))"), "\n24\n");
}

#[test]
fn multiplication_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function *))))"), "\n1\n");
}

#[test]
fn multiplication_test_indirect_3() {
  assert_eq!(parse_and_run("((print (funcall (function *) 3)))"), "\n3\n");
}

#[test]
fn subtraction_test_1() {
  assert_eq!(parse_and_run("((print (- 4)))"), "\n-4\n");
}

#[test]
fn subtraction_test_2() {
  assert_eq!(parse_and_run("((print (- 4 3)))"), "\n1\n");
}

#[test]
fn subtraction_test_3() {
  assert_eq!(parse_and_run("((print (- 4 3 2)))"), "\n-1\n");
}

#[test]
fn subtraction_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function -) 4 3 2)))"), "\n-1\n");
}

#[test]
fn subtraction_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function -) 3)))"), "\n-3\n");
}

#[test]
fn division_test_1() {
  assert_eq!(parse_and_run("((print (/ 4)))"), "\n0.25\n");
}

#[test]
fn division_test_2() {
  assert_eq!(parse_and_run("((print (/ 4 2)))"), "\n2\n");
}

#[test]
fn division_test_3() {
  assert_eq!(parse_and_run("((print (/ 4 2 2)))"), "\n1\n");
}

#[test]
fn division_test_4() {
  assert_eq!(parse_and_run("((print (/ (vector 4 2 4) 2 2)))"), "\n(1, 0.5, 1)\n");
}

#[test]
fn division_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function /) 4)))"), "\n0.25\n");
}

#[test]
fn division_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function /) 3 2)))"), "\n1.5\n");
}

#[test]
fn int_division_test_1() {
  assert_eq!(parse_and_run("((print (div 4)))"), "\n0\n");
}

#[test]
fn int_division_test_2() {
  assert_eq!(parse_and_run("((print (div 4 2)))"), "\n2\n");
}

#[test]
fn int_division_test_3() {
  assert_eq!(parse_and_run("((print (div 4 2 2)))"), "\n1\n");
}

#[test]
fn int_division_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function div) 4)))"), "\n0\n");
}

#[test]
fn int_division_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function div) 3 2)))"), "\n1\n");
}

#[test]
fn eq_test_1() {
  assert_eq!(parse_and_run("((print (= 4)))"), "\nTrue\n");
}

#[test]
fn eq_test_2() {
  assert_eq!(parse_and_run("((print (= 4 4)))"), "\nTrue\n");
}

#[test]
fn eq_test_3() {
  assert_eq!(parse_and_run("((print (= 2 2 4)))"), "\nFalse\n");
}

#[test]
fn eq_test_stateful_1() {
  assert_eq!(parse_and_run("((print (= (print 1))))"), "\n1\nTrue\n");
}

#[test]
fn eq_test_stateful_2() {
  assert_eq!(parse_and_run("((print (= (print 1) (print 2) (print 3))))"), "\n1\n2\n3\nTrue\n");
}

#[test]
fn eq_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function =) 1)))"), "\nTrue\n");
}

#[test]
fn eq_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function =) 1 2)))"), "\nFalse\n");
}

#[test]
fn eq_test_indirect_3() {
  assert_eq!(parse_and_run("((print (funcall (function =) 1 1 1)))"), "\nTrue\n");
}

#[test]
fn lt_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function <) 1 2 3)))"), "\nTrue\n");
}

#[test]
fn gt_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function >) 3 2 1)))"), "\nTrue\n");
}

#[test]
fn le_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function <=) 1 2 2 3)))"), "\nTrue\n");
}

#[test]
fn ge_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function >=) 3 2 2 1)))"), "\nTrue\n");
}

#[test]
fn ne_test_1() {
  assert_eq!(parse_and_run("((print (/= 1 1 1)))"), "\nFalse\n");
}

#[test]
fn ne_test_2() {
  assert_eq!(parse_and_run("((print (/= 1 3 2)))"), "\nTrue\n");
}

#[test]
fn ne_test_3() {
  assert_eq!(parse_and_run("((print (/= 1 2 1)))"), "\nFalse\n");
}

#[test]
fn not_test_1() {
  assert_eq!(parse_and_run("((print (not #t)))"), "\nFalse\n");
}

#[test]
fn not_test_2() {
  assert_eq!(parse_and_run("((print (not #f)))"), "\nTrue\n");
}

#[test]
pub fn addition_compile_test() {
  assert_eq!(parse_compile_and_output("(+)"), "return 0\n");
  assert_eq!(parse_compile_and_output("(+ 1)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(+ 1 2)"), "return 1 + 2\n");
  assert_eq!(parse_compile_and_output("(+ 1 2 3)"), "return 1 + 2 + 3\n");
}

#[test]
pub fn multiplication_compile_test() {
  assert_eq!(parse_compile_and_output("(*)"), "return 1\n");
  assert_eq!(parse_compile_and_output("(* 2)"), "return 2\n");
  assert_eq!(parse_compile_and_output("(* 2 3)"), "return 2 * 3\n");
  assert_eq!(parse_compile_and_output("(* 2 3 4)"), "return 2 * 3 * 4\n");
}

#[test]
pub fn subtraction_compile_test() {
  assert_eq!(parse_compile_and_output("(- 2)"), "return -2\n");
  assert_eq!(parse_compile_and_output("(- 2 3)"), "return 2 - 3\n");
  assert_eq!(parse_compile_and_output("(- 2 3 4)"), "return 2 - 3 - 4\n");
}

#[test]
pub fn division_compile_test() {
  assert_eq!(parse_compile_and_output("(/ 2)"), "return 1 / float(2)\n");
  assert_eq!(parse_compile_and_output("(/ 2 3)"), "return 2 / float(3)\n");
  assert_eq!(parse_compile_and_output("(/ 2 3 4)"), "return 2 / float(3) / float(4)\n");
  assert_eq!(parse_compile_and_output("(/ 2.0 3 4.0)"), "return 2e0 / float(3) / 4e0\n");
}

#[test]
pub fn int_division_compile_test() {
  assert_eq!(parse_compile_and_output("(div 2)"), "return 1 / 2\n");
  assert_eq!(parse_compile_and_output("(div 2 3)"), "return 2 / 3\n");
  assert_eq!(parse_compile_and_output("(div 2 3 4)"), "return 2 / 3 / 4\n");
  assert_eq!(parse_compile_and_output("(div foobar 3 4)"), "return foobar / 3 / 4\n");
}

#[test]
pub fn eq_compile_test() {
  assert_eq!(parse_compile_and_output("(= 1)"), "return true\n");
  assert_eq!(parse_compile_and_output("(= 1 2)"), "return 1 == 2\n");
  assert_eq!(parse_compile_and_output("(= 1 2 3)"), "return 1 == 2 && 2 == 3\n");
}

#[test]
pub fn eq_compile_test_stateful() {
  assert_eq!(parse_compile_and_output("(= (foo))"), "foo()\nreturn true\n");
  assert_eq!(parse_compile_and_output("(= (foo) (foo))"), "return foo() == foo()\n");
  assert_eq!(parse_compile_and_output("(= (foo) (foo) (foo))"), "var _cmp_0 = foo()\nvar _cmp_1 = foo()\nvar _cmp_2 = foo()\nreturn _cmp_0 == _cmp_1 && _cmp_1 == _cmp_2\n");
}

#[test]
pub fn cmp_compile_test() {
  assert_eq!(parse_compile_and_output("(< 1 2)"), "return 1 < 2\n");
  assert_eq!(parse_compile_and_output("(> 1 2 3)"), "return 1 > 2 && 2 > 3\n");
  assert_eq!(parse_compile_and_output("(<= 1 2)"), "return 1 <= 2\n");
  assert_eq!(parse_compile_and_output("(>= 1 2 3)"), "return 1 >= 2 && 2 >= 3\n");
}

#[test]
pub fn cmp_compile_test_stateful() {
  assert_eq!(parse_compile_and_output("(< (foo))"), "foo()\nreturn true\n");
  assert_eq!(parse_compile_and_output("(<= (foo) (foo))"), "return foo() <= foo()\n");
  assert_eq!(parse_compile_and_output("(> (foo) (foo) (foo))"), "var _cmp_0 = foo()\nvar _cmp_1 = foo()\nvar _cmp_2 = foo()\nreturn _cmp_0 > _cmp_1 && _cmp_1 > _cmp_2\n");
  assert_eq!(parse_compile_and_output("(>= (foo) (foo))"), "return foo() >= foo()\n");
}

#[test]
pub fn ne_compile_test() {
  assert_eq!(parse_compile_and_output("(/= 1)"), "return true\n");
  assert_eq!(parse_compile_and_output("(/= (foo))"), "foo()\nreturn true\n");
  assert_eq!(parse_compile_and_output("(/= 1 2)"), "return 1 != 2\n");
  assert_eq!(parse_compile_and_output("(/= 1 2 3)"), "return GDLisp._DIV__EQ_(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, null)))\n");
}

#[test]
pub fn array_subscript_test() {
  assert_eq!(parse_compile_and_output("(elt 1 2)"), "return 1[2]\n");
}

#[test]
pub fn array_subscript_test_run() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (print (elt x 2))))"), "\n30\n");
}

#[test]
pub fn array_subscript_test_run_indirect() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (print (funcall #'elt x 2))))"), "\n30\n");
}

#[test]
pub fn array_subscript_assign_test_run_1() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (set (elt x 1) 999) (print x)))"), "\n[10, 999, 30]\n");
}

#[test]
pub fn array_subscript_assign_test_run_2() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (set-elt 999 x 1) (print x)))"), "\n[10, 999, 30]\n");
}

#[test]
pub fn array_subscript_assign_test_run_3() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (funcall #'set-elt 999 x 1) (print x)))"), "\n[10, 999, 30]\n");
}

#[test]
pub fn min_max_test_1() {
  assert_eq!(parse_compile_and_output("(min 0 1)"), "return min(0, 1)\n");
  assert_eq!(parse_compile_and_output("(max 0 1)"), "return max(0, 1)\n");
}

#[test]
pub fn min_max_test_2() {
  assert_eq!(parse_compile_and_output("(min 0)"), "return 0\n");
  assert_eq!(parse_compile_and_output("(max 0)"), "return 0\n");
}

#[test]
pub fn min_max_test_3() {
  assert_eq!(parse_compile_and_output("(min)"), "return INF\n");
  assert_eq!(parse_compile_and_output("(max)"), "return -INF\n");
}

#[test]
pub fn min_max_test_4() {
  assert_eq!(parse_compile_and_output("(min 1 2 3)"), "return min(min(1, 2), 3)\n");
  assert_eq!(parse_compile_and_output("(max 1 2 3)"), "return max(max(1, 2), 3)\n");
}

#[test]
pub fn min_max_run_test() {
  assert_eq!(parse_and_run("((print (min 1 2 3)))"), "\n1\n");
  assert_eq!(parse_and_run("((print (max 1 2 3)))"), "\n3\n");
}
