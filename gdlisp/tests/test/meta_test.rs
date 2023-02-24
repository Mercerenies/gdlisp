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
use gdlisp::gdscript::metadata;

#[test]
fn meta_test_cons() {
  let code = format!(r#"((let ((arg (cons 1 2))) (print (bool (arg:get-meta "{}")))))"#, metadata::CONS_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_cons_quoted() {
  let code = format!(r#"((let ((arg '(1 . 2))) (print (bool (arg:get-meta "{}")))))"#, metadata::CONS_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_symbol() {
  let code = format!(r#"((let ((arg (intern "sym"))) (print (bool (arg:get-meta "{}")))))"#, metadata::SYMBOL_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_symbol_quoted() {
  let code = format!(r#"((let ((arg 'sym)) (print (bool (arg:get-meta "{}")))))"#, metadata::SYMBOL_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_symbol_not_on_cons() {
  let code = format!(r#"((let ((arg '(1 . 2))) (print (arg:has-meta "{}"))))"#, metadata::SYMBOL_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nFalse\n");
}

#[test]
fn meta_test_cons_not_on_symbol() {
  let code = format!(r#"((let ((arg 'sym)) (print (arg:has-meta "{}"))))"#, metadata::CONS_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nFalse\n");
}
