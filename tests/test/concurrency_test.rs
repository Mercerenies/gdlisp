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

use std::thread;

// Mostly failed attempts at reproducing #44, but the test is still
// technically good, so I'm leaving it.
#[test]
fn concurrent_runs_test() {

  fn thread_fn(_n: u32) {
    assert_eq!(parse_compile_and_output("(if 1 2 3)"),
               "var _cond = null\nif 1:\n    _cond = 2\nelse:\n    if true:\n        _cond = 3\n    else:\n        _cond = null\nreturn _cond\n");
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
