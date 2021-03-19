
use super::common::{parse_compile_and_output_h, parse_and_run};

#[test]
pub fn basic_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(new Node)");
  assert_eq!(result0.0, "return _AnonymousClass_0.new()\n");
  assert_eq!(result0.1, r#"class _AnonymousClass_0 extends Node:
    func _init():
        pass
"#);

  let result1 = parse_compile_and_output_h("(new Node (defn foo (x) (+ x 1)))");
  assert_eq!(result1.0, "return _AnonymousClass_0.new()\n");
  assert_eq!(result1.1, r#"class _AnonymousClass_0 extends Node:
    func _init():
        pass
    func foo(x_1):
        return x_1 + 1
"#);

}

#[test]
pub fn constructor_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(new Node (defvar x) (defn _init () (setq self:x 1)) (defn foo () self:x))");
  assert_eq!(result0.0, "return _AnonymousClass_0.new()\n");
  assert_eq!(result0.1, r#"class _AnonymousClass_0 extends Node:
    func _init():
        self.x = 1
    var x
    func foo():
        return self.x
"#);

}

#[test]
pub fn closure_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(let ((a 1)) (new Node (defn foo (x) (+ x a))))");
  assert_eq!(result0.0, "var a_0 = 1\nreturn _AnonymousClass_1.new(a_0)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass_1 extends Node:
    func _init(a_0):
        self.a_0 = a_0
    var a_0
    func foo(x_2):
        return x_2 + a_0
"#);

}

#[test]
#[ignore]
pub fn lambda_class_running_test_1() {
  let output = parse_and_run("((let ((x (new Reference (defn foo () 100)))) (print (x:foo))))");
  assert_eq!(output, "\n100\n");
}

#[test]
#[ignore]
pub fn lambda_class_running_test_2() {
  let output = parse_and_run("((let ((x (let ((y 99)) (new Reference (defn foo () y))))) (print (x:foo))))");
  assert_eq!(output, "\n99\n");
}

#[test]
#[ignore]
pub fn lambda_class_running_test_3() {
  let output = parse_and_run(r#"
    ((let ((inst (let ((x 768))
                   (flet ((f () x))
                     (new Reference (defn foo () (f)))))))
      (print (inst:foo))))
  "#);
  assert_eq!(output, "\n768\n");
}

#[test]
#[ignore]
pub fn lambda_class_running_test_4() {
  let output = parse_and_run(r#"
    ((let ((inst (let ((x 768))
                   (labels ((f () (if #f (f) x)))
                     (new Reference (defn foo () (funcall #'f)))))))
      (print (inst:foo))))
  "#);
  assert_eq!(output, "\n768\n");
}

#[test]
#[ignore]
pub fn lambda_class_running_test_5() {
  // Note: This one requires x to be wrapped in a Cell.
  let output = parse_and_run(r#"
    ((let ((x 1))
       (let ((foo (new Reference (defn increment () (setq x (+ x 1))))))
         (print x)
         (print (foo:increment))
         (print x))))
  "#);
  assert_eq!(output, "\n1\n2\n2\n");
}
