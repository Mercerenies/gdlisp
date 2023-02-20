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
fn array_roundtrip_test() {
  assert_eq!(parse_and_run("
  ((let ((arr [10 20 30 40]))
     (let ((arr1 (list->array (array->list arr))))
       (print (elt arr1 0))
       (print (elt arr1 1))
       (print (elt arr1 2))
       (print (elt arr1 3)))))
   "), "\n10\n20\n30\n40\n");
}

#[test]
fn array_list_length_test() {
  assert_eq!(parse_and_run("((print (len (array->list [9 10 11]))))"), "\n3\n");
}

#[test]
fn list_array_length_test() {
  assert_eq!(parse_and_run("((print (len (list->array '(9 10 11)))))"), "\n3\n");
}

#[test]
fn array_varargs_test_1() {
  assert_eq!(parse_and_run("((defn foo (&arr arr) (elt arr 0)) (print (foo 10 20 30)))"),
             "\n10\n");
}

#[test]
fn array_varargs_test_2() {
  assert_eq!(parse_and_run("((let ((foo (lambda (&arr arr) (elt arr 0)))) (print (funcall foo 10 20 30))))"),
             "\n10\n");
}
