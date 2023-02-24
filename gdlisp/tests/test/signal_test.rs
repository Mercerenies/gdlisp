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
pub fn basic_connect_to_signal_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defsignal frobnicated))
     (let ((foo (Foo:new)))
       (connect>> foo "frobnicated" (lambda () (print "Received")))
       (print "A")
       (foo:emit-signal "frobnicated")
       (print "B")))
  "#), "\nA\nReceived\nB\n");
}

#[test]
pub fn signal_multiple_fires_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defsignal frobnicated))
     (let ((foo (Foo:new)))
       (connect>> foo "frobnicated" (lambda () (print "Received")))
       (print "A")
       (foo:emit-signal "frobnicated")
       (foo:emit-signal "frobnicated")
       (foo:emit-signal "frobnicated")
       (print "B")))
  "#), "\nA\nReceived\nReceived\nReceived\nB\n");
}

#[test]
pub fn signal_multiple_fires_oneshot_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defsignal frobnicated))
     (let ((foo (Foo:new)))
       (connect1>> foo "frobnicated" (lambda () (print "Received")))
       (print "A")
       (foo:emit-signal "frobnicated")
       (foo:emit-signal "frobnicated")
       (foo:emit-signal "frobnicated")
       (print "B")))
  "#), "\nA\nReceived\nB\n");
}

#[test]
pub fn signal_connect_disconnect_test() {
  assert_eq!(parse_and_run(r#"
    ((defclass Foo (Reference)
       (defsignal frobnicated))
     (let ((foo (Foo:new)))
       (let ((index (connect>> foo "frobnicated" (lambda () (print "1")))))
         (print "A")
         (foo:emit-signal "frobnicated")
         (print "B")
         (disconnect>> foo "frobnicated" index)
         (foo:emit-signal "frobnicated")
         (connect>> foo "frobnicated" (lambda () (print "2")))
         (print "C")
         (foo:emit-signal "frobnicated")
         (print "D"))))
  "#), "\nA\n1\nB\nC\n2\nD\n");
}
