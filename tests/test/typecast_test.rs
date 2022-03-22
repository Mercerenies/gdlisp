
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
  assert_eq!(parse_compile_and_output("(bool 10)"), "return bool(10)\n");
}

#[test]
pub fn char_test() {
  assert_eq!(parse_compile_and_output("(char 10)"), "return char(10)\n");
}
