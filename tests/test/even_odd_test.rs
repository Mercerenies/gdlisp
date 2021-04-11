
use super::common::parse_and_run;

#[test]
#[ignore]
fn even_odd_test_labels() {
  let output = parse_and_run(r#"
  ((labels ((is-even (x) (if (= x 0) #t (is-odd  (- x 1))))
            (is-odd  (x) (if (= x 0) #f (is-even (- x 1)))))
    (let ((x 0))
      (while (< x 11)
        (print (is-even x))
        (print (is-odd x))
        (set x (+ x 1))))))
  "#);
  assert_eq!(output, "\nTrue\nFalse\nFalse\nTrue\nTrue\nFalse\nFalse\nTrue\nTrue\nFalse\nFalse\nTrue\nTrue\nFalse\nFalse\nTrue\nTrue\nFalse\nFalse\nTrue\nTrue\nFalse\n");
}
