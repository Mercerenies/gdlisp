
use super::common::parse_and_run;

#[test]
#[ignore]
fn array_roundtrip_test() {
  assert_eq!(parse_and_run("
  ((let ((arr [10 20 30 40]))
     (let ((arr1 (list->array (array->list arr))))
       (print (elt arr1 0))
       (print (elt arr1 1))
       (print (elt arr1 2))
       (print (elt arr1 3)))))
   "), "\n10\n20\n30\n40\n");
}

#[test]
#[ignore]
fn array_list_length_test() {
  assert_eq!(parse_and_run("((print (length (array->list [9 10 11]))))"), "\n3\n");
}

#[test]
#[ignore]
fn array_varargs_test_1() {
  assert_eq!(parse_and_run("((defn foo (&arr arr) (elt arr 0)) (print (foo 10 20 30)))"),
             "\n10\n");
}

#[test]
#[ignore]
fn array_varargs_test_2() {
  assert_eq!(parse_and_run("((let ((foo (lambda (&arr arr) (elt arr 0)))) (print (funcall foo 10 20 30))))"),
             "\n10\n");
}
