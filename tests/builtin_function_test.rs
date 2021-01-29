
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
fn addition_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall (function +) 1 5 2 -4)))"), "\n4\n");
  assert_eq!(parse_and_run("((print (funcall (function +))))"), "\n0\n");
  assert_eq!(parse_and_run("((print (funcall (function +) 3)))"), "\n3\n");
}
