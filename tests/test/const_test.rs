
use super::common::{parse_compile_decl, parse_and_run};

#[test]
pub fn const_test() {
  assert_eq!(parse_compile_decl("((defconst A 10))"), "extends Reference\nconst A = 10\nstatic func run():\n    return null\n");
  assert_eq!(parse_compile_decl("((defconst A \"foo\"))"), "extends Reference\nconst A = \"foo\"\nstatic func run():\n    return null\n");
}

#[test]
#[should_panic]
pub fn const_test_nonconst_1() {
  parse_compile_decl("((defconst B ()))");
}

#[test]
#[should_panic]
pub fn const_test_nonconst_2() {
  parse_compile_decl("((defconst B (list->array 1)))");
}

#[test]
#[ignore]
fn const_test_run() {
  let output = parse_and_run(r#"
  ((defconst A 100) (print A))
  "#);
  assert_eq!(output, "\n100\n");
}
