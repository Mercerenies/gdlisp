
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::ir::identifier::ClassNamespace;

use super::common::*;

#[test]
pub fn basic_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(new Node)");
  assert_eq!(result0.0, "return _AnonymousClass.new()\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init():
        pass
"#);

  let result1 = parse_compile_and_output_h("(new Node (defn foo (x) (+ x 1)))");
  assert_eq!(result1.0, "return _AnonymousClass.new()\n");
  assert_eq!(result1.1, r#"class _AnonymousClass extends Node:

    func _init():
        pass

    func foo(x):
        return x + 1
"#);

}

#[test]
pub fn constructor_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(new Node (defvar x) (defn _init () (set self:x 1)) (defn foo () self:x))");
  assert_eq!(result0.0, "return _AnonymousClass.new()\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init():
        self.x = 1

    var x

    func foo():
        return self.x
"#);

}

#[test]
pub fn constructor_args_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(new (Node 99) (defvar x) (defvar y) (defn _init (y) (set self:x 1) (set self:y y)) (defn foo () [self:x self:y]))");
  assert_eq!(result0.0, "return _AnonymousClass.new(99)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(y):
        self.x = 1
        self.y = y

    var x
    var y

    func foo():
        return [self.x, self.y]
"#);

}

#[test]
pub fn constructor_init_args_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(new (Node 99) (defvar x) (defvar y) (defn _init (@x y) (set self:y y)) (defn foo () [self:x self:y]))");
  assert_eq!(result0.0, "return _AnonymousClass.new(99)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(x_0, y):
        self.x = x_0
        self.y = y

    var x
    var y

    func foo():
        return [self.x, self.y]
"#);

}

#[test]
pub fn closure_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(let ((a 1)) (new Node (defn foo (x) (+ x a))))");
  assert_eq!(result0.0, "var a = 1\nreturn _AnonymousClass.new(a)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(a):
        self.a = a

    var a

    func foo(x):
        return x + a
"#);

}

#[test]
pub fn closure_and_args_lambda_class_test() {

  let result0 = parse_compile_and_output_h("(let ((a 1)) (new (Node 77) (defvar z) (defn _init (z) (set self:z z)) (defn foo () (+ self:z a))))");
  assert_eq!(result0.0, "var a = 1\nreturn _AnonymousClass.new(a, 77)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(a, z):
        self.a = a
        self.z = z

    var a
    var z

    func foo():
        return self.z + a
"#);

}

#[test]
pub fn closure_and_init_args_lambda_class_test_1() {

  let result0 = parse_compile_and_output_h("(let ((a 1)) (new (Node 77) (defvar z) (defvar t) (defn _init (z @t) (set self:z z)) (defn foo () (+ self:z a))))");
  assert_eq!(result0.0, "var a = 1\nreturn _AnonymousClass.new(a, 77)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(a, z, t_0):
        self.a = a
        self.t = t_0
        self.z = z

    var a
    var z
    var t

    func foo():
        return self.z + a
"#);

}

#[test]
pub fn closure_and_init_args_lambda_class_test_2() {

  let result0 = parse_compile_and_output_h("(let ((a 1)) (new (Node 77) (defvar w (sys/split 1)) (defvar z) (defvar t) (defn _init (z @t) (set self:z z)) (defn foo () (+ self:z a))))");
  assert_eq!(result0.0, "var a = 1\nreturn _AnonymousClass.new(a, 77)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(a, z, t_0):
        var _split_0 = 1
        self.w = _split_0
        self.a = a
        self.t = t_0
        self.z = z

    var a
    var w
    var z
    var t

    func foo():
        return self.z + a
"#);

}

#[test]
pub fn closure_and_args_lambda_class_with_name_conflict_test() {

  let result0 = parse_compile_and_output_h("(let ((z 1)) (new (Node 77) (defvar z) (defn _init (z) (set self:z z)) (defn foo () (+ self:z z))))");
  assert_eq!(result0.0, "var z = 1\nreturn _AnonymousClass.new(z, 77)\n");
  assert_eq!(result0.1, r#"class _AnonymousClass extends Node:

    func _init(z_0, z_1):
        self.z_0 = z_0
        self.z = z_1

    var z_0
    var z

    func foo():
        return self.z + z_0
"#);

}

#[test]
pub fn closure_and_args_lambda_class_with_argument_name_conflict_test() {

  let result0 = parse_compile_decl("((defn foo (z) (new (Node 77) (defvar z) (defn _init (z) (set self:z z)) (defn foo () (+ self:z z)))))");
  assert_eq!(result0, r#"extends Reference


class _AnonymousClass extends Node:

    func _init(z_0, z_1):
        self.z_0 = z_0
        self.z = z_1

    var z_0
    var z

    func foo():
        return self.z + z_0


static func foo(z):
    return _AnonymousClass.new(z, 77)
"#);

}

#[test]
pub fn capture_self_lambda_class_test() {
  let result = parse_compile_decl("((defclass Foo (Reference) (defn f () (new Reference (defn g () self)))))");
  assert_eq!(result, r#"extends Reference


class _AnonymousClass extends Reference:

    func _init():
        pass

    func g():
        return self


class Foo extends Reference:

    func _init():
        pass

    func f():
        return _AnonymousClass.new()
"#);
}

#[test]
pub fn constructor_uses_outer_ref_lambda_class_test() {
  let result = parse_compile_decl("((defn foo () 1) (defn bar () (new Reference (defn _init () (foo)))))");
  assert_eq!(result, r#"extends Reference


static func foo():
    return 1


class _AnonymousClass extends Reference:

    func _init():
        __gdlisp_outer_class_0.foo()

    var __gdlisp_outer_class_0 = load("res://TEST.gd")


static func bar():
    return _AnonymousClass.new()
"#);
}

#[test]
pub fn constructor_uses_outer_ref_lambda_class_test_run() {
  let result = parse_and_run("((defn foo () (print 10)) (new Reference (defn _init () (foo))))");
  assert_eq!(result, "\n10\n");
}

#[test]
pub fn bad_static_in_lambda_class_test() {
  assert_eq!(
    parse_compile_and_output_err("(new (Node) (defn example () static 1))"),
    Err(PError::from(GDError::new(GDErrorF::StaticOnLambdaClass(String::from("example")), SourceOffset(13)))),
  );
}

#[test]
pub fn bad_const_in_lambda_class_test() {
  assert_eq!(
    parse_compile_and_output_err("(new (Node) (defconst FOO 1))"),
    Err(PError::from(GDError::new(GDErrorF::StaticOnLambdaClass(String::from("FOO")), SourceOffset(13)))),
  );
}

#[test]
pub fn lambda_class_running_test_1() {
  let output = parse_and_run("((let ((x (new Reference (defn foo () 100)))) (print (x:foo))))");
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn lambda_class_running_test_2() {
  let output = parse_and_run("((let ((x (let ((y 99)) (new Reference (defn foo () y))))) (print (x:foo))))");
  assert_eq!(output, "\n99\n");
}

#[test]
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
pub fn lambda_class_running_test_5() {
  // Note: This one requires x to be wrapped in a Cell.
  let output = parse_and_run(r#"
    ((let ((x 1))
       (let ((foo (new Reference (defn increment () (set x (+ x 1))))))
         (print x)
         (print (foo:increment))
         (print x))))
  "#);
  assert_eq!(output, "\n1\n2\n2\n");
}

#[test]
pub fn lambda_class_running_test_6() {
  let output = parse_and_run("((let ((x (let ((y 99)) (new (Reference 1) (defvar z) (defn _init (z) (set self:z z)) (defn foo () (+ self:z y)))))) (print (x:foo))))");
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn lambda_class_access_static_fn_test_1() {
  let output = parse_and_run("((defn foo () 100) (let ((x (new Reference (defn test () (foo))))) (print (x:test))))");
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn lambda_class_access_static_fn_test_2() {
  let output = parse_and_run("((defn foo () 100) (let ((x (new Reference (defvar x) (defn _init () (set self:x (foo)))))) (print x:x)))");
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn lambda_class_super_test_1() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference)
       (defn foo ()
         99))
     (let ((x (new Foo (defn foo () (+ (super:foo) 2)))))
       (print (x:foo))))
  "#);
  assert_eq!(output, "\n101\n");
}

#[test]
pub fn lambda_class_super_test_2() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference)
       (defn foo ()
         99))
     (let ((x (new Foo (defn foo () (lambda () (+ 2 (super:foo)))))))
       (print (funcall (x:foo)))))
  "#);
  assert_eq!(output, "\n101\n");
}

#[test]
pub fn lambda_class_init_test() {
  let output = parse_and_run(r#"
    ((defclass Foo (Reference)
       (defvar x)
       (defn _init (@x)))
     (let ((foo (new (Foo 100) (defn _init (x) (super x)))))
       (print foo:x)))
  "#);
  assert_eq!(output, "\n100\n");
}

#[test]
pub fn lambda_class_duplicate_constructor_test() {
  assert_eq!(
    parse_compile_decl_err("((defn function-name () (new Node (defn _init ()) (defn _init ()))))"),
    Err(PError::from(GDError::new(GDErrorF::DuplicateConstructor, SourceOffset(51)))),
  );
}

#[test]
pub fn lambda_class_duplicate_name_test() {
  assert_eq!(
    parse_compile_decl_err("((defn function-name () (new Node (defn foo ()) (defn foo ()))))"),
    Err(PError::from(GDError::new(GDErrorF::DuplicateName(ClassNamespace::Function, String::from("foo")), SourceOffset(49)))),
  );
}
