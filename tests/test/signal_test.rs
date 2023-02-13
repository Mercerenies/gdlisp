
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
