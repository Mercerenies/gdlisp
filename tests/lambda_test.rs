
mod common;

use common::parse_and_run;

#[test]
#[ignore]
fn simple_lambda_test() {
  let output = parse_and_run("((print (funcall (lambda (a) a) 1)))");
  assert_eq!(output, "\n1\n");
}
