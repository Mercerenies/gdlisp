
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
