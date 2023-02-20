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
fn factorial_test_global() {
  let output = parse_and_run(r#"
  ((defn fact (x)
     (if (<= x 1)
       1
       (* x (fact (- x 1)))))
    (let ((x 0))
      (while (< x 6)
        (print (fact x))
        (set x (+ x 1)))))
  "#);
  assert_eq!(output, "\n1\n1\n2\n6\n24\n120\n");
}

#[test]
fn factorial_test_labels() {
  let output = parse_and_run(r#"
  ((labels ((fact (x)
              (if (<= x 1)
                1
                (* x (fact (- x 1))))))
     (let ((x 0))
        (while (< x 6)
           (print (fact x))
           (set x (+ x 1))))))
  "#);
  assert_eq!(output, "\n1\n1\n2\n6\n24\n120\n");
}
