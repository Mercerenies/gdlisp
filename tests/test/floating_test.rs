
use super::common::parse_and_run;

#[test]
fn floating_test() {
  // I'm using Rust's built-in to_string on f32 to get floating-point
  // output. I want to make sure nothing funny happens on the Godot
  // side when I do that.
  let output = parse_and_run(r#"
    ((let (x)
      (set x 1.0000)
      (set x -0.29199)
      (set x 3e19)
      (set x +3e19)
      (set x -4.1E3)
      (set x -4.1E+3)
      (set x 1e-2)))
  "#);
  assert_eq!(output, "\n");
}
