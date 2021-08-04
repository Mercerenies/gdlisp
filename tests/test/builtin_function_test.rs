
use gdlisp::compile::error::{Error as GDError};
use gdlisp::pipeline::error::{Error as PError};

use super::common::*;

#[test]
#[ignore]
fn addition_test_1() {
  assert_eq!(parse_and_run("((print (+)))"), "\n0\n");
}

#[test]
#[ignore]
fn addition_test_2() {
  assert_eq!(parse_and_run("((print (+ 1)))"), "\n1\n");
}

#[test]
#[ignore]
fn addition_test_3() {
  assert_eq!(parse_and_run("((print (+ 1 5)))"), "\n6\n");
}

#[test]
#[ignore]
fn addition_test_4() {
  assert_eq!(parse_and_run("((print (+ 1 5 2)))"), "\n8\n");
}

#[test]
#[ignore]
fn addition_test_5() {
  assert_eq!(parse_and_run("((print (+ 1 5 2 -4)))"), "\n4\n");
}

#[test]
#[ignore]
fn addition_test_6() {
  assert_eq!(parse_and_run("((print (+ (vector 1 1) (vector 2 3))))"), "\n(3, 4)\n");
}

#[test]
#[ignore]
fn addition_test_7() {
  assert_eq!(parse_and_run("((print (+ (vector 1 1))))"), "\n(1, 1)\n");
}

#[test]
#[ignore]
fn addition_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function +) 1 5 2 -4)))"), "\n4\n");
}

#[test]
#[ignore]
fn addition_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function +))))"), "\n0\n");
}

#[test]
#[ignore]
fn addition_test_indirect_3() {
  assert_eq!(parse_and_run("((print (funcall (function +) 3)))"), "\n3\n");
}

#[test]
#[ignore]
fn multiplication_test_1() {
  assert_eq!(parse_and_run("((print (*)))"), "\n1\n");
}

#[test]
#[ignore]
fn multiplication_test_2() {
  assert_eq!(parse_and_run("((print (* 4)))"), "\n4\n");
}

#[test]
#[ignore]
fn multiplication_test_3() {
  assert_eq!(parse_and_run("((print (* 4 3)))"), "\n12\n");
}

#[test]
#[ignore]
fn multiplication_test_4() {
  assert_eq!(parse_and_run("((print (* 4 3 2)))"), "\n24\n");
}

#[test]
#[ignore]
fn multiplication_test_5() {
  assert_eq!(parse_and_run("((print (* 4 (vector 1 2) 2 (vector 2 3))))"), "\n(16, 48)\n");
}

#[test]
#[ignore]
fn multiplication_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function *) 4 3 2)))"), "\n24\n");
}

#[test]
#[ignore]
fn multiplication_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function *))))"), "\n1\n");
}

#[test]
#[ignore]
fn multiplication_test_indirect_3() {
  assert_eq!(parse_and_run("((print (funcall (function *) 3)))"), "\n3\n");
}

#[test]
#[ignore]
fn subtraction_test_1() {
  assert_eq!(parse_and_run("((print (- 4)))"), "\n-4\n");
}

#[test]
#[ignore]
fn subtraction_test_2() {
  assert_eq!(parse_and_run("((print (- 4 3)))"), "\n1\n");
}

#[test]
#[ignore]
fn subtraction_test_3() {
  assert_eq!(parse_and_run("((print (- 4 3 2)))"), "\n-1\n");
}

#[test]
#[ignore]
fn subtraction_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function -) 4 3 2)))"), "\n-1\n");
}

#[test]
#[ignore]
fn subtraction_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function -) 3)))"), "\n-3\n");
}

#[test]
#[ignore]
fn division_test_1() {
  assert_eq!(parse_and_run("((print (/ 4)))"), "\n0.25\n");
}

#[test]
#[ignore]
fn division_test_2() {
  assert_eq!(parse_and_run("((print (/ 4 2)))"), "\n2\n");
}

#[test]
#[ignore]
fn division_test_3() {
  assert_eq!(parse_and_run("((print (/ 4 2 2)))"), "\n1\n");
}

#[test]
#[ignore]
fn division_test_4() {
  assert_eq!(parse_and_run("((print (/ (vector 4 2 4) 2 2)))"), "\n(1, 0.5, 1)\n");
}

#[test]
#[ignore]
fn division_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function /) 4)))"), "\n0.25\n");
}

#[test]
#[ignore]
fn division_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function /) 3 2)))"), "\n1.5\n");
}

#[test]
#[ignore]
fn int_division_test_1() {
  assert_eq!(parse_and_run("((print (div 4)))"), "\n0\n");
}

#[test]
#[ignore]
fn int_division_test_2() {
  assert_eq!(parse_and_run("((print (div 4 2)))"), "\n2\n");
}

#[test]
#[ignore]
fn int_division_test_3() {
  assert_eq!(parse_and_run("((print (div 4 2 2)))"), "\n1\n");
}

#[test]
#[ignore]
fn int_division_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function div) 4)))"), "\n0\n");
}

#[test]
#[ignore]
fn int_division_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function div) 3 2)))"), "\n1\n");
}

#[test]
#[ignore]
fn eq_test_1() {
  assert_eq!(parse_and_run("((print (= 4)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn eq_test_2() {
  assert_eq!(parse_and_run("((print (= 4 4)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn eq_test_3() {
  assert_eq!(parse_and_run("((print (= 2 2 4)))"), "\nFalse\n");
}

#[test]
#[ignore]
fn eq_test_stateful_1() {
  assert_eq!(parse_and_run("((print (= (print 1))))"), "\n1\nTrue\n");
}

#[test]
#[ignore]
fn eq_test_stateful_2() {
  assert_eq!(parse_and_run("((print (= (print 1) (print 2) (print 3))))"), "\n1\n2\n3\nTrue\n");
}

#[test]
#[ignore]
fn eq_test_indirect_1() {
  assert_eq!(parse_and_run("((print (funcall (function =) 1)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn eq_test_indirect_2() {
  assert_eq!(parse_and_run("((print (funcall (function =) 1 2)))"), "\nFalse\n");
}

#[test]
#[ignore]
fn eq_test_indirect_3() {
  assert_eq!(parse_and_run("((print (funcall (function =) 1 1 1)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn lt_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function <) 1 2 3)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn gt_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function >) 3 2 1)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn le_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function <=) 1 2 2 3)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn ge_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function >=) 3 2 2 1)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn ne_test_1() {
  assert_eq!(parse_and_run("((print (/= 1 1 1)))"), "\nFalse\n");
}

#[test]
#[ignore]
fn ne_test_2() {
  assert_eq!(parse_and_run("((print (/= 1 3 2)))"), "\nTrue\n");
}

#[test]
#[ignore]
fn ne_test_3() {
  assert_eq!(parse_and_run("((print (/= 1 2 1)))"), "\nFalse\n");
}

#[test]
#[ignore]
fn not_test_1() {
  assert_eq!(parse_and_run("((print (not #t)))"), "\nFalse\n");
}

#[test]
#[ignore]
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
  assert_eq!(parse_compile_and_output("(/= 1 2 3)"), "return GDLisp._u002F_EQ_(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, null)))\n");
}

#[test]
pub fn simple_length_test() {
  assert_eq!(parse_compile_and_output("(length ())"), "return GDLisp.length(null)\n");
}

#[test]
pub fn list_test() {
  assert_eq!(parse_compile_and_output("(list 1 2 3)"), "return GDLisp.Cons.new(1, GDLisp.Cons.new(2, GDLisp.Cons.new(3, null)))\n");
}

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
pub fn array_test() {
  assert_eq!(parse_compile_and_output("[]"), "return []\n");
  assert_eq!(parse_compile_and_output("[1 2 3]"), "return [1, 2, 3]\n");
  assert_eq!(parse_compile_and_output("[2]"), "return [2]\n");
  assert_eq!(parse_compile_and_output("[(foo)]"), "return [foo()]\n");
  assert_eq!(parse_compile_and_output("(progn [1] [2])"), "return [2]\n");
  assert_eq!(parse_compile_and_output("(progn [(foo)] [2])"), "[foo()]\nreturn [2]\n");
  assert_eq!(parse_compile_and_output("[(if 1 2 3)]"), "var _cond_0 = null\nif 1:\n    _cond_0 = 2\nelse:\n    if true:\n        _cond_0 = 3\n    else:\n        _cond_0 = null\nreturn [_cond_0]\n");
}

#[test]
pub fn yield_test() {
  assert_eq!(parse_compile_and_output("(yield)"), "return yield()\n");
  assert_eq!(parse_compile_and_output("(yield 1 2)"), "return yield(1, 2)\n");
}

#[test]
pub fn array_subscript_test() {
  assert_eq!(parse_compile_and_output("(elt 1 2)"), "return 1[2]\n");
}

#[test]
#[ignore]
pub fn array_subscript_test_run() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (print (elt x 2))))"), "\n30\n");
}

#[test]
#[ignore]
pub fn array_subscript_test_run_indirect() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (print (funcall #'elt x 2))))"), "\n30\n");
}

#[test]
#[ignore]
pub fn array_subscript_assign_test_run_1() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (set (elt x 1) 999) (print x)))"), "\n[10, 999, 30]\n");
}

#[test]
#[ignore]
pub fn array_subscript_assign_test_run_2() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (set-elt 999 x 1) (print x)))"), "\n[10, 999, 30]\n");
}

#[test]
#[ignore]
pub fn array_subscript_assign_test_run_3() {
  assert_eq!(parse_and_run("((let ((x [10 20 30])) (funcall #'set-elt 999 x 1) (print x)))"), "\n[10, 999, 30]\n");
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
    Err(PError::from(GDError::NoSuchVar(String::from("NotARealClass")))),
  );
}

#[test]
pub fn return_test() {
  assert_eq!(parse_compile_and_output("(progn (if 1 (return 2)) 3)"),
             "if 1:\n    return 2\nelse:\n    if true:\n        pass\n    else:\n        pass\nreturn 3\n");
}

#[test]
#[ignore]
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
#[ignore]
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
#[ignore]
pub fn instance_check_test() {
  let result = parse_and_run(r#"
    ((print (instance? 1 Int))
     (print (instance? 1.0 Float))
     (print (instance? 1 Float))
     (print (instance? (Reference:new) Object))
     (print (instance? (Reference:new) Reference))
     (print (instance? (Reference:new) Node))
     (print (instance? (Reference:new) Array)))
  "#);
  assert_eq!(result, "\nTrue\nTrue\nFalse\nTrue\nTrue\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn map_test_1() {
  let result = parse_and_run(r#"
    ((print (map (lambda (x) (+ x 1)) [4 5 6])))
  "#);
  assert_eq!(result, "\n[5, 6, 7]\n");
}

#[test]
#[ignore]
pub fn map_test_2() {
  let result = parse_and_run(r#"
    ((let ((x (map (lambda (x) (+ x 1)) '(4 5 6))))
       (print (length x))
       (print x:car)
       (print x:cdr:car)
       (print x:cdr:cdr:car)))
  "#);
  assert_eq!(result, "\n3\n5\n6\n7\n");
}

#[test]
#[ignore]
pub fn filter_test_1() {
  let result = parse_and_run(r#"
    ((print (filter (lambda (x) (= (mod x 2) 0)) [1 2 3 4 5 6])))
  "#);
  assert_eq!(result, "\n[2, 4, 6]\n");
}

#[test]
#[ignore]
pub fn filter_test_2() {
  let result = parse_and_run(r#"
    ((let ((x (filter (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5 6))))
       (print (length x))
       (print x:car)
       (print x:cdr:car)
       (print x:cdr:cdr:car)))
  "#);
  assert_eq!(result, "\n3\n2\n4\n6\n");
}

#[test]
#[ignore]
pub fn filter_test_3() {
  let result = parse_and_run(r#"
    ((let ((x (filter (lambda (x) #f) '(1 2 3 4 5 6))))
       (print (length x))))
  "#);
  assert_eq!(result, "\n0\n");
}

#[test]
#[ignore]
pub fn append_test_1() {
  let result = parse_and_run(r#"
    ((print (list->array (append))))
  "#);
  assert_eq!(result, "\n[]\n");
}

#[test]
#[ignore]
pub fn append_test_2() {
  let result = parse_and_run(r#"
    ((print (list->array (append '(1 2 3 4)))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4]\n");
}

#[test]
#[ignore]
pub fn append_test_3() {
  let result = parse_and_run(r#"
    ((print (list->array (append '(1 2 3 4) '(5 6 7 8)))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4, 5, 6, 7, 8]\n");
}

#[test]
#[ignore]
pub fn append_test_4() {
  let result = parse_and_run(r#"
    ((print (list->array (append '(1 2 3 4) () '(5 6 7 8) () ()))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4, 5, 6, 7, 8]\n");
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
    Err(PError::from(GDError::NoSuchMagic(String::from("THIS-MAGIC-DOES-NOT-EXIST")))),
  );
}

// TODO Test gensym at runtime once we can pretty-print symbols
