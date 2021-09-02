
use super::common::*;

#[test]
#[ignore]
pub fn instance_check_test() {
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
