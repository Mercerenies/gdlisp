
use super::common::*;

use std::thread;
use std::time::Duration;

#[test]
#[ignore]
fn concurrent_runs_test() {

  fn thread_fn(n: u32) {
    assert_eq!(parse_compile_and_output("(if 1 2 3)"),
               "var _cond_0 = null\nif 1:\n    _cond_0 = 2\nelse:\n    if true:\n        _cond_0 = 3\n    else:\n        _cond_0 = null\nreturn _cond_0\n");
    assert_eq!(parse_and_run("((let ((r (Reference:new))) (print (= (typeof r) Reference))))"),
               "\nTrue\n");
  }

  let runs = 4;
  let join_handles: Vec<_> = (0..runs).map(|n| thread::spawn(move || thread_fn(n))).collect();

  // Let them run concurrently
  thread::yield_now();

  // Now join the handles
  for handle in join_handles {
    handle.join().unwrap();
  }

}
