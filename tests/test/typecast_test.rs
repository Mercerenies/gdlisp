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

use super::common::*;

#[test]
pub fn int_test() {
  assert_eq!(parse_compile_and_output("(int 10)"), "return int(10)\n");
}

#[test]
pub fn int_running_test() {
  assert_eq!(parse_and_run("((print (int 10.5)))"), "\n10\n");
  assert_eq!(parse_and_run("((print (int \"4\")))"), "\n4\n");
}

#[test]
pub fn str_test() {
  assert_eq!(parse_compile_and_output("(str 10)"), "return str(10)\n");
}

#[test]
pub fn str_running_test() {
  assert_eq!(parse_and_run("((print (str 10.5)))"), "\n10.5\n");
  assert_eq!(parse_and_run("((print (str 4)))"), "\n4\n");
  assert_eq!(parse_and_run("((print (str \"foo\")))"), "\nfoo\n");
  assert_eq!(parse_and_run("((print (str nil)))"), "\nNull\n");
}

#[test]
pub fn bool_test() {
  // We wrap bool(...) to support more types.
  assert_eq!(parse_compile_and_output("(bool 10)"), "return GDLisp._bool(10)\n");
}

#[test]
pub fn char_test() {
  assert_eq!(parse_compile_and_output("(char 10)"), "return char(10)\n");
}

#[test]
pub fn ord_test() {
  assert_eq!(parse_compile_and_output("(ord \"A\")"), "return ord(\"A\")\n");
}

#[test]
pub fn bool_running_test() {
  assert_eq!(parse_and_run("((print (bool 10))
                             (print (bool 0))
                             (print (bool (Reference:new))))"),
             "\nTrue\nFalse\nTrue\n");
}
