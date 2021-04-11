
use super::common::parse_and_run;

#[test]
#[ignore]
fn factorial_test_global() {
  let output = parse_and_run(r#"
  ((defn fact (x)
     (if (<= x 1)
       1
       (* x (fact (- x 1)))))
    (let ((x 0))
      (while (< x 6)
        (print (fact x))
        (set x (+ x 1)))))
  "#);
  assert_eq!(output, "\n1\n1\n2\n6\n24\n120\n");
}

#[test]
#[ignore]
fn factorial_test_labels() {
  let output = parse_and_run(r#"
  ((labels ((fact (x)
              (if (<= x 1)
                1
                (* x (fact (- x 1))))))
     (let ((x 0))
        (while (< x 6)
           (print (fact x))
           (set x (+ x 1))))))
  "#);
  assert_eq!(output, "\n1\n1\n2\n6\n24\n120\n");
}
