
mod common;

use common::parse_and_run;

#[test]
#[ignore]
fn simple_lambda_test() {
  let output = parse_and_run("((print (funcall (lambda (a) a) 1)))");
  assert_eq!(output, "\n1\n");
}

#[test]
#[ignore]
fn closure_lambda_test() {
  let output = parse_and_run(r#"
  ((let ((a (let ((x 99))
              (let ((f (lambda (z) x)))
                f))))
    (print (funcall a ()))))
  "#);
  assert_eq!(output, "\n99\n");
}

// This fails right now! /////
#[test]
#[ignore]
fn modifying_closure_test() {
  let output = parse_and_run(r#"
  ((let ((a 0))
    (funcall (lambda () (setq a 1)))
    (print a)))
  "#);
  assert_eq!(output, "\n1\n");
}