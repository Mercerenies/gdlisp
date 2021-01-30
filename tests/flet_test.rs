
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

#[test]
#[ignore]
fn closed_flet_test_1() {
  let output = parse_and_run(r#"
    ((let ((x 0))
       (flet ((f () (setq x (+ x 1))))
         (print (f))
         (print (f)))))"#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
#[ignore]
fn closed_flet_test_2() {
  let output = parse_and_run(r#"
    ((let ((g (let ((x 0))
                (flet ((f () (setq x (+ x 1))))
                  (lambda () (f))))))
       (print (funcall g))
       (print (funcall g))))
  "#);
  assert_eq!(output, "\n1\n2\n");
}

#[test]
#[ignore]
fn nested_flet_test() {
  let output = parse_and_run(r#"
    ((let ((g (let ((x 0))
                (flet ((f () (setq x (+ x 1))))
                  (flet ((g () (f)))
                    (function g))))))
       (print (funcall g))
       (print (funcall g))))
  "#);
  assert_eq!(output, "\n1\n2\n");
}
