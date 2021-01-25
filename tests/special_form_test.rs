
mod common;

use common::parse_and_run;

#[test]
#[ignore]
fn if_test_1() {
  let output = parse_and_run(r#"
  ((let ((x 1))
    (print (if x 10 20))))
  "#);
  assert_eq!(output, "\n10\n");
}

#[test]
#[ignore]
fn if_test_2() {
  let output = parse_and_run(r#"
  ((let ((x 0))
    (print (if x 10 20))))
  "#);
  assert_eq!(output, "\n20\n");
}
