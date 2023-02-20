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
