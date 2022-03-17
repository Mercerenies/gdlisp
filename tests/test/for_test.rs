
use super::common::{parse_compile_and_output, parse_and_run};

#[test]
fn for_loop_test() {
  let output = parse_and_run(r#"
    ((for variable 3 (print variable)))
  "#);
  assert_eq!(output, "\n0\n1\n2\n");
}

#[test]
pub fn for_tests() {
  assert_eq!(parse_compile_and_output("(for x 1)"), "for x_0 in 1:\n    pass\nreturn null\n");
  assert_eq!(parse_compile_and_output("(for x 1 2)"), "for x_0 in 1:\n    pass\nreturn null\n");
  assert_eq!(parse_compile_and_output("(for x 1 (foo))"), "for x_0 in 1:\n    foo()\nreturn null\n");
  assert_eq!(parse_compile_and_output("(for x-y 1 (foo))"), "for x_y_0 in 1:\n    foo()\nreturn null\n");
}
