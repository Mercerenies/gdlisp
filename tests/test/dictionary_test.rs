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

use super::common::{parse_compile_and_output, parse_and_run};

#[test]
fn dict_test_1() {
  assert_eq!(parse_compile_and_output("{}"), "return {}\n");
}

#[test]
fn dict_test_2() {
  assert_eq!(parse_compile_and_output("{1 2 3 4}"), "return {1: 2, 3: 4}\n");
}

#[test]
fn dict_test_running() {
  let result = parse_and_run(r#"((let ((a {"b" 3})) (print (dict/elt a "b"))))"#);
  assert_eq!(result, "\n3\n");
}

#[test]
fn dict_test_set_running() {
  let result = parse_and_run(r#"((let ((a {"b" 3}))
                                   (set (dict/elt a "c") 100)
                                   (print (dict/elt a "b"))
                                   (print (dict/elt a "c"))
                                   (set (dict/elt a "b") -3)
                                   (print (dict/elt a "b"))))"#);
  assert_eq!(result, "\n3\n100\n-3\n");
}
