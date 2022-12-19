
use super::common::parse_and_run;
use gdlisp::gdscript::metadata;

#[test]
fn meta_test_cons() {
  let code = format!(r#"((let ((arg (cons 1 2))) (print (bool (arg:get-meta "{}")))))"#, metadata::CONS_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_cons_quoted() {
  let code = format!(r#"((let ((arg '(1 . 2))) (print (bool (arg:get-meta "{}")))))"#, metadata::CONS_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_symbol() {
  let code = format!(r#"((let ((arg (intern "sym"))) (print (bool (arg:get-meta "{}")))))"#, metadata::SYMBOL_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_symbol_quoted() {
  let code = format!(r#"((let ((arg 'sym)) (print (bool (arg:get-meta "{}")))))"#, metadata::SYMBOL_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nTrue\n");
}

#[test]
fn meta_test_symbol_not_on_cons() {
  let code = format!(r#"((let ((arg '(1 . 2))) (print (arg:has-meta "{}"))))"#, metadata::SYMBOL_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nFalse\n");
}

#[test]
fn meta_test_cons_not_on_symbol() {
  let code = format!(r#"((let ((arg 'sym)) (print (arg:has-meta "{}"))))"#, metadata::CONS_META);
  let output = parse_and_run(&code);
  assert_eq!(output, "\nFalse\n");
}
