
mod common;

use common::parse_and_run;

#[test]
#[ignore]
fn for_loop_test() {
  let output = parse_and_run(r#"
    ((for variable 3 (print variable)))
  "#);
  assert_eq!(output, "\n0\n1\n2\n");
}
