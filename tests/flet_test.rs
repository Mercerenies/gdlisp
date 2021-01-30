
mod common;

use common::parse_and_run;

#[test]
#[ignore]
fn simple_flet_test_1() {
  let output = parse_and_run("((flet ((f (x) x)) (print (f 1))))");
  assert_eq!(output, "\n1\n");
}

#[test]
#[ignore]
fn simple_flet_test_2() {
  let output = parse_and_run("((flet ((f (x) (* x 2))) (print (f 1)) (print (f 2))))");
  assert_eq!(output, "\n2\n4\n");
}
