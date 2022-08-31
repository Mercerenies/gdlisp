
use super::common::*;

#[test]
pub fn simple_length_test() {
  assert_eq!(parse_compile_and_output("(len ())"), "return GDLisp._len(null)\n");
}

#[test]
pub fn list_test() {
  assert_eq!(parse_compile_and_output("(list 1 2 3)"), "return GDLisp.cons(1, GDLisp.cons(2, GDLisp.cons(3, null)))\n");
}

#[test]
pub fn array_test() {
  assert_eq!(parse_compile_and_output("[]"), "return []\n");
  assert_eq!(parse_compile_and_output("[1 2 3]"), "return [1, 2, 3]\n");
  assert_eq!(parse_compile_and_output("[2]"), "return [2]\n");
  assert_eq!(parse_compile_and_output("[(foo)]"), "return [foo()]\n");
  assert_eq!(parse_compile_and_output("(progn [1] [2])"), "return [2]\n");
  assert_eq!(parse_compile_and_output("(progn [(foo)] [2])"), "[foo()]\nreturn [2]\n");
  assert_eq!(parse_compile_and_output("[(if 1 2 3)]"), "var _cond = null\nif 1:\n    _cond = 2\nelse:\n    if true:\n        _cond = 3\n    else:\n        _cond = null\nreturn [_cond]\n");
}

#[test]
pub fn map_test_1() {
  let result = parse_and_run(r#"
    ((print (map (lambda (x) (+ x 1)) [4 5 6])))
  "#);
  assert_eq!(result, "\n[5, 6, 7]\n");
}

#[test]
pub fn map_test_2() {
  let result = parse_and_run(r#"
    ((let ((x (map (lambda (x) (+ x 1)) '(4 5 6))))
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
    ((print (filter (lambda (x) (= (mod x 2) 0)) [1 2 3 4 5 6])))
  "#);
  assert_eq!(result, "\n[2, 4, 6]\n");
}

#[test]
pub fn filter_test_2() {
  let result = parse_and_run(r#"
    ((let ((x (filter (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5 6))))
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
    ((let ((x (filter (lambda (x) #f) '(1 2 3 4 5 6))))
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
pub fn list_tail_test_1() {
  let result = parse_and_run(r#"
    ((print (list->array (list-tail '(1 2 3 4) 0))))
  "#);
  assert_eq!(result, "\n[1, 2, 3, 4]\n");
}

#[test]
pub fn list_tail_test_2() {
  let result = parse_and_run(r#"
    ((print (list->array (list-tail '(1 2 3 4) 2))))
  "#);
  assert_eq!(result, "\n[3, 4]\n");
}
