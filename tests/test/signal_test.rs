
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::runner::version::{get_godot_version, Version};

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

///// test more and then document these new functions
