
use super::common::parse_and_run;

#[test]
#[ignore]
fn simple_labels_test_1() {
  let output = parse_and_run("((labels ((f (x) x)) (print (f 1))))");
  assert_eq!(output, "\n1\n");
}
