
mod common;

use common::parse_and_run;

#[test]
#[ignore] // Not working yet :)
fn simple_lambda_test() {
  let _output = parse_and_run("((print (funcall (lambda (a) a) 1)))");
  ///// Get the output (make sure it's printing) and test it
}
