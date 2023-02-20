// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

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
