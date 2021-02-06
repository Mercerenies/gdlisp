
mod common;

use common::parse_and_run;

#[test]
#[ignore]
fn floating_test() {
  // I'm using Rust's built-in to_string on f32 to get floating-point
  // output. I want to make sure nothing funny happens on the Godot
  // side when I do that.
  let output = parse_and_run(r#"
    ((let (x)
      (setq x 1.0000)
      (setq x -0.29199)
      (setq x 3e19)
      (setq x +3e19)
      (setq x -4.1E3)
      (setq x -4.1E+3)
      (setq x 1e-2)))
  "#);
  assert_eq!(output, "\n");
}
