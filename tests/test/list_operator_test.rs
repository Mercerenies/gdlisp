// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

use super::common::*;
use gdlisp::pipeline::error::PError;

#[test]
pub fn simple_length_test() {
  assert_eq!(parse_compile_and_output("(len ())"), "return GDLisp._len(null)\n");
}

#[test]
pub fn list_test() {
  assert_eq!(parse_compile_and_output("(list 1 2 3)"), "return GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, null)))\n");
}

#[test]
pub fn array_function_test() {
  assert_eq!(parse_compile_and_output("(array 1 2 3)"), "return [1, 2, 3]\n");
}

#[test]
pub fn array_test() {
  assert_eq!(parse_compile_and_output("[]"), "return []\n");
  assert_eq!(parse_compile_and_output("[1 2 3]"), "return [1, 2, 3]\n");
  assert_eq!(parse_compile_and_output("[2]"), "return [2]\n");
  assert_eq!(parse_compile_and_output("[(foo)]"), "return [foo()]\n");
  assert_eq!(parse_compile_and_output("(progn [(foo)] [2])"), "[foo()]\nreturn [2]\n");
  assert_eq!(parse_compile_and_output("[(if 1 2 3)]"), "var _cond = null\nif 1:\n    _cond = 2\nelse:\n    if true:\n        _cond = 3\n    else:\n        _cond = null\nreturn [_cond]\n");
}

#[test]
pub fn reverse_test_1() {
  let result = parse_and_run(r#"
    ((print (array/reverse [1 2 3 4])))
  "#);
  assert_eq!(result, "\n[4, 3, 2, 1]\n");
}

#[test]
pub fn reverse_test_2() {
  let result = parse_and_run(r#"
    ((let ((list (list/reverse '(1 2 3 4))))
       (print (len list))
       (print (list/elt list 0))
       (print (list/elt list 1))
       (print (list/elt list 2))
       (print (list/elt list 3))))
  "#);
  assert_eq!(result, "\n4\n4\n3\n2\n1\n");
}

#[test]
pub fn map_test_1() {
  let result = parse_and_run(r#"
    ((print (array/map (lambda (x) (+ x 1)) [4 5 6])))
  "#);
  assert_eq!(result, "\n[5, 6, 7]\n");
}

#[test]
pub fn map_test_2() {
  let result = parse_and_run(r#"
    ((let ((x (list/map (lambda (x) (+ x 1)) '(4 5 6))))
       (print (len x))
       (print x:car)
       (print x:cdr:car)
       (print x:cdr:cdr:car)))
  "#);
  assert_eq!(result, "\n3\n5\n6\n7\n");
}

#[test]
pub fn filter_test_1() {
  let result = parse_and_run(r#"
    ((print (array/filter (lambda (x) (= (mod x 2) 0)) [1 2 3 4 5 6])))
  "#);
  assert_eq!(result, "\n[2, 4, 6]\n");
}

#[test]
pub fn filter_test_2() {
  let result = parse_and_run(r#"
    ((let ((x (list/filter (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5 6))))
       (print (len x))
       (print x:car)
       (print x:cdr:car)
       (print x:cdr:cdr:car)))
  "#);
  assert_eq!(result, "\n3\n2\n4\n6\n");
}

#[test]
pub fn filter_test_3() {
  let result = parse_and_run(r#"
    ((let ((x (list/filter (lambda (x) #f) '(1 2 3 4 5 6))))
       (print (len x))))
  "#);
  assert_eq!(result, "\n0\n");
}

#[test]
pub fn length_test() {
  assert_eq!(parse_and_run("((print (len nil)))"), "\n0\n");
  assert_eq!(parse_and_run("((print (len '(1 2 3 4))))"), "\n4\n");
  assert_eq!(parse_and_run("((print (len [1 2 3 4])))"), "\n4\n");
}

#[test]
pub fn append_test_1() {
  let result = parse_and_run(r#"
    ((print (list->array (append))))
  "#);
  assert_eq!(result, "\n[]\n");
}

#[test]
pub fn append_test_2() {
  let result = parse_and_run(r#"
    ((print (list->array (append '(1 2 3 4)))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn append_test_3() {
  let result = parse_and_run(r#"
    ((print (list->array (append '(1 2 3 4) '(5 6 7 8)))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4, 5, 6, 7, 8]\n");
}

#[test]
pub fn append_test_4() {
  let result = parse_and_run(r#"
    ((print (list->array (append '(1 2 3 4) () '(5 6 7 8) () ()))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4, 5, 6, 7, 8]\n");
}

#[test]
pub fn array_running_test() {
  let result = parse_and_run(r#"
    ((print (array 1 2 3)))
  "#);
  assert_eq!(result, "\n[1, 2, 3]\n");
}

#[test]
pub fn array_syntax_running_test() {
  let result = parse_and_run(r#"
    ((print [1 2 3]))
  "#);
  assert_eq!(result, "\n[1, 2, 3]\n");
}

#[test]
pub fn array_running_test_indirect() {
  let result = parse_and_run(r#"
    ((print (funcall #'array 1 2 3)))
  "#);
  assert_eq!(result, "\n[1, 2, 3]\n");
}

#[test]
pub fn dict_running_test() {
  let result = parse_and_run(r#"
    ((print (dict 1 2 3 4)))
  "#);
  assert_eq!(result, "\n{1:2, 3:4}\n");
}

#[test]
pub fn dict_syntax_running_test() {
  let result = parse_and_run(r#"
    ((print {1 2 3 4}))
  "#);
  assert_eq!(result, "\n{1:2, 3:4}\n");
}

#[test]
pub fn dict_running_test_indirect() {
  let result = parse_and_run(r#"
    ((print (funcall #'dict 1 2 3 4)))
  "#);
  assert_eq!(result, "\n{1:2, 3:4}\n");
}

#[test]
pub fn dict_syntax_odd_error_test() {
  let result = parse_compile_and_output_err(r#"
    ((print {1}))
  "#);
  assert!(matches!(result, Err(PError::ParseError(_))));
}

#[test]
pub fn list_tail_test_1() {
  let result = parse_and_run(r#"
    ((print (list->array (list/tail '(1 2 3 4) 0))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn list_tail_test_2() {
  let result = parse_and_run(r#"
    ((print (list->array (list/tail '(1 2 3 4) 2))))
  "#);
  assert_eq!(result, "\n[3, 4]\n");
}

#[test]
pub fn list_elt_test_1() {
  let result = parse_and_run(r#"
    ((print (list/elt '(1 2 3 4) 0)))
  "#);
  assert_eq!(result, "\n1\n");
}

#[test]
pub fn list_elt_test_2() {
  let result = parse_and_run(r#"
    ((print (list/elt '(1 2 3 4) 2)))
  "#);
  assert_eq!(result, "\n3\n");
}

#[test]
pub fn list_foreach_test() {
  let result = parse_and_run(r#"
    ((let ((foo '(1 2 3 4 5))
           (total 0))
       (list/for x foo
         (set total (+ total x)))
       (print total)))
  "#);
  assert_eq!(result, "\n15\n");
}

#[test]
pub fn fold_test_1() {
  let result = parse_and_run(r#"
    ((let ((foo '(1 2 3 4 5)))
       (print (list/fold #'+ foo))
       (print (list/fold #'+ foo 100))))
  "#);
  assert_eq!(result, "\n15\n115\n");
}

#[test]
pub fn fold_test_2() {
  let result = parse_and_run(r#"
    ((let ((foo [1 2 3 4 5]))
       (print (array/fold #'+ foo))
       (print (array/fold #'+ foo 100))))
  "#);
  assert_eq!(result, "\n15\n115\n");
}
