
mod common;

use common::parse_and_run;

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
