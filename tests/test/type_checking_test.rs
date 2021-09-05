
use super::common::*;

#[test]
#[ignore]
pub fn primitive_instance_check_test_1() {
  let result = parse_and_run(r#"
    ((print (instance? 1 Int))
     (print (instance? 1.0 Float))
     (print (instance? 1 Float))
     (print (instance? (Reference:new) Object))
     (print (instance? (Reference:new) Reference))
     (print (instance? (Reference:new) Node))
     (print (instance? (Reference:new) Array)))
  "#);
  assert_eq!(result, "\nTrue\nTrue\nFalse\nTrue\nTrue\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn primitive_instance_check_test_2() {
  let result = parse_and_run(r#"
    ((print (instance? V{1 2} Vector3))
     (print (instance? V{1 2 3} Vector3))
     (print (instance? V{1 2} Vector2))
     (print (instance? V{1 2 3} Vector2))
     (print (instance? 0 Vector2))
     (print (instance? "A" Vector3)))
  "#);
  assert_eq!(result, "\nFalse\nTrue\nTrue\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn object_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node Object))
      (print (instance? (Reference:new) Object))
      (print (instance? 3 Object))
      (print (instance? [3] Object))
      (print (instance? {"a" 3} Object))
      (print (instance? nil Object))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nTrue\nTrue\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn reference_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node Reference))
      (print (instance? (Reference:new) Reference))
      (print (instance? 3 Reference))
      (print (instance? [3] Reference))
      (print (instance? {"a" 3} Reference))
      (print (instance? nil Reference))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nFalse\nTrue\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn node_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node Node))
      (print (instance? (Reference:new) Node))
      (print (instance? 3 Node))
      (print (instance? [3] Node))
      (print (instance? {"a" 3} Node))
      (print (instance? nil Node))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nTrue\nFalse\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn custom_reference_instance_check_test() {
  let result = parse_and_run(r#"
    ((defclass Foo (Reference))
     (let ((my-node (Node:new)))
      (print (instance? my-node Foo))
      (print (instance? (Reference:new) Foo))
      (print (instance? (Foo:new) Foo))
      (print (instance? 3 Foo))
      (print (instance? [3] Foo))
      (print (instance? {"a" 3} Foo))
      (print (instance? nil Foo))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nTrue\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn custom_node_instance_check_test() {
  let result = parse_and_run(r#"
    ((defclass Foo (Node))
     (let ((my-node (Node:new))
           (my-foo (Foo:new)))
      (print (instance? my-node Foo))
      (print (instance? (Reference:new) Foo))
      (print (instance? my-foo Foo))
      (print (instance? 3 Foo))
      (print (instance? [3] Foo))
      (print (instance? {"a" 3} Foo))
      (print (instance? nil Foo))
      (my-node:free)
      (my-foo:free)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nTrue\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn number_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node Number))
      (print (instance? (Reference:new) Number))
      (print (instance? 3 Number))
      (print (instance? 3.1 Number))
      (print (instance? [3] Number))
      (print (instance? {"a" 3} Number))
      (print (instance? nil Number))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nTrue\nTrue\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn any_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node Any))
      (print (instance? (Reference:new) Any))
      (print (instance? 3 Any))
      (print (instance? 3.1 Any))
      (print (instance? [3] Any))
      (print (instance? {"a" 3} Any))
      (print (instance? nil Any))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nTrue\nTrue\nTrue\nTrue\nTrue\nTrue\nTrue\n");
}

#[test]
#[ignore]
pub fn anyref_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node AnyRef))
      (print (instance? (Reference:new) AnyRef))
      (print (instance? 3 AnyRef))
      (print (instance? 3.1 AnyRef))
      (print (instance? [3] AnyRef))
      (print (instance? {"a" 3} AnyRef))
      (print (instance? nil AnyRef))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nTrue\nTrue\nFalse\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn anyval_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node AnyVal))
      (print (instance? (Reference:new) AnyVal))
      (print (instance? 3 AnyVal))
      (print (instance? 3.1 AnyVal))
      (print (instance? [3] AnyVal))
      (print (instance? {"a" 3} AnyVal))
      (print (instance? nil AnyVal))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nTrue\nTrue\nTrue\nTrue\nTrue\n");
}

#[test]
#[ignore]
pub fn nothing_instance_check_test() {
  let result = parse_and_run(r#"
    ((let ((my-node (Node:new)))
      (print (instance? my-node Nothing))
      (print (instance? (Reference:new) Nothing))
      (print (instance? 3 Nothing))
      (print (instance? 3.1 Nothing))
      (print (instance? [3] Nothing))
      (print (instance? {"a" 3} Nothing))
      (print (instance? nil Nothing))
      (my-node:free)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nFalse\nFalse\nFalse\nFalse\nFalse\n");
}

#[test]
#[ignore]
pub fn array_instance_check_test() {
  let result = parse_and_run(r#"
    ((print (instance? (Reference:new) Array))
     (print (instance? 3 Array))
     (print (instance? [3] Array))
     (print (instance? ((literally PoolIntArray) [3]) Array)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nTrue\nFalse\n");
}

#[test]
#[ignore]
pub fn specific_array_instance_check_test() {
  let result = parse_and_run(r#"
    ((print (instance? (Reference:new) PoolIntArray))
     (print (instance? 3 PoolIntArray))
     (print (instance? [3] PoolIntArray))
     (print (instance? ((literally PoolIntArray) [3]) PoolIntArray)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nFalse\nTrue\n");
}

#[test]
#[ignore]
pub fn base_array_instance_check_test() {
  let result = parse_and_run(r#"
    ((print (instance? (Reference:new) BaseArray))
     (print (instance? 3 BaseArray))
     (print (instance? [3] BaseArray))
     (print (instance? ((literally PoolIntArray) [3]) BaseArray)))
  "#);
  assert_eq!(result, "\nFalse\nFalse\nTrue\nTrue\n");
}
