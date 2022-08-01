
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;

use super::common::*;

#[test]
pub fn vector_test() {
  assert_eq!(parse_compile_and_output("(vector 9 10)"), "return Vector2(9, 10)\n");
  assert_eq!(parse_compile_and_output("(vector 9 10 11)"), "return Vector3(9, 10, 11)\n");
}

#[test]
pub fn vector_syntax_test() {
  assert_eq!(parse_compile_and_output("V{9 10}"), "return Vector2(9, 10)\n");
  assert_eq!(parse_compile_and_output("V{9 10 11}"), "return Vector3(9, 10, 11)\n");
}

#[test]
pub fn yield_test() {
  assert_eq!(parse_compile_and_output("(yield)"), "return yield()\n");
  assert_eq!(parse_compile_and_output("(yield 1 2)"), "return yield(1, 2)\n");
}

#[test]
pub fn attribute_test() {
  assert_eq!(parse_compile_and_output("(let ((foo 1)) foo:bar)"), "var foo = 1\nreturn foo.bar\n");
  assert_eq!(parse_compile_and_output("(let ((foo 1)) foo:bar 2)"), "var foo = 1\nreturn 2\n");
}

#[test]
pub fn method_test() {
  assert_eq!(parse_compile_and_output("(let ((foo 1)) (foo:bar))"), "var foo = 1\nreturn foo.bar()\n");
  assert_eq!(parse_compile_and_output("(let ((foo 1)) (foo:bar 100) 2)"), "var foo = 1\nfoo.bar(100)\nreturn 2\n");
}

#[test]
pub fn simple_builtin_test() {
  assert_eq!(parse_compile_and_output("(cons 1 2)"), "return GDLisp.cons(1, 2)\n");
  assert_eq!(parse_compile_and_output("(cons 1 (cons 2 3))"), "return GDLisp.cons(1, GDLisp.cons(2, 3))\n");
  assert_eq!(parse_compile_and_output("(intern 10)"), "return GDLisp.intern(10)\n");
}

#[test]
pub fn known_gdscript_classes_test_1() {
  assert_eq!(parse_compile_and_output("[Sprite Node Node2D GDScript Object]"),
             "return [Sprite, Node, Node2D, GDScript, GDLisp._Object]\n");
}

#[test]
pub fn known_gdscript_classes_test_2() {
  // Note: They all get checked, but all except the last is elided by the statefulness rules.
  assert_eq!(parse_compile_and_output("(progn Sprite Node Node2D GDScript Object)"), "return GDLisp._Object\n");
}

#[test]
pub fn unknown_gdscript_classes_test() {
  assert_eq!(
    parse_compile_and_output_err("(progn NotARealClass Node2D GDScript Object)"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchVar(String::from("NotARealClass")), SourceOffset(7)))),
  );
}

#[test]
pub fn return_test() {
  assert_eq!(parse_compile_and_output("(progn (if 1 (return 2)) 3)"),
             "if 1:\n    return 2\nelse:\n    if true:\n        pass\n    else:\n        pass\nreturn 3\n");
}

#[test]
pub fn yield_running_test() {
  let result = parse_and_run(r#"
    ((defn foo ()
       (print 2)
       (yield)
       (print 4)
       6)
     (print 1)
     (let ((x (foo)))
       (print 3)
       (let ((final (x:resume)))
         (print 5)
         (print final))))
  "#);
  assert_eq!(result, "\n1\n2\n3\n4\n5\n6\n");
}

#[test]
pub fn yield_star_running_test() {
  let result = parse_and_run(r#"
    ((defn foo ()
       (print 2)
       (yield)
       (print 4)
       6)
     (defn bar ()
       (yield* (foo)))
     (print 1)
     (let ((x (bar)))
       (print 3)
       (let ((final (x:resume)))
         (print 5)
         (print final))))
  "#);
  assert_eq!(result, "\n1\n2\n3\n4\n5\n6\n");
}

#[test]
pub fn custom_call_magic_test() {
  assert_eq!(parse_compile_decl("((defn foo (x y) (sys/call-magic ADDITION) 9) (foo 10 20))"),
             r#"extends Reference
static func foo(x, y):
    return 9
static func run():
    return 10 + 20
"#);
}

#[test]
pub fn custom_call_magic_test_failed() {
  assert_eq!(
    parse_compile_decl_err("((defn foo (x y) (sys/call-magic THIS-MAGIC-DOES-NOT-EXIST) 9))"),
    Err(PError::from(GDError::new(GDErrorF::NoSuchMagic(String::from("THIS-MAGIC-DOES-NOT-EXIST")), SourceOffset(2)))),
  );
}

#[test]
pub fn split_call_test_1() {
  assert_eq!(parse_compile_and_output("(car (car (car (sys/split (car 0)))))"),
             r#"var _split = GDLisp.car(0)
return GDLisp.car(GDLisp.car(GDLisp.car(_split)))
"#);
}

#[test]
pub fn split_call_test_2() {
  assert_eq!(parse_compile_and_output("(car (car (sys/split (car (sys/split (car 0))))))"),
             r#"var _split = GDLisp.car(0)
var _split_0 = GDLisp.car(_split)
return GDLisp.car(GDLisp.car(_split_0))
"#);
}

#[test]
pub fn wrapped_random_functions_test() {
  assert_eq!(parse_compile_and_output("(randomize)"), "return randomize()\n");
  assert_eq!(parse_compile_and_output("(randi)"), "return randi()\n");
  assert_eq!(parse_compile_and_output("(randf)"), "return randf()\n");
  assert_eq!(parse_compile_and_output("(rand-range 0 1)"), "return rand_range(0, 1)\n");
  assert_eq!(parse_compile_and_output("(seed 999)"), "return seed(999)\n");
  assert_eq!(parse_compile_and_output("(rand-seed 999)"), "return rand_seed(999)\n");
}

#[test]
pub fn wrapped_math_functions_test() {
  assert_eq!(parse_compile_and_output("(clamp 0 1 2)"), "return clamp(0, 1, 2)\n");
  assert_eq!(parse_compile_and_output("(abs 0)"), "return abs(0)\n");
  assert_eq!(parse_compile_and_output("(acos 0)"), "return acos(0)\n");
  assert_eq!(parse_compile_and_output("(asin 0)"), "return asin(0)\n");
  assert_eq!(parse_compile_and_output("(atan 0)"), "return atan(0)\n");
  assert_eq!(parse_compile_and_output("(atan2 0 1)"), "return atan2(0, 1)\n");
  assert_eq!(parse_compile_and_output("(cos 0)"), "return cos(0)\n");
  assert_eq!(parse_compile_and_output("(cosh 0)"), "return cosh(0)\n");
  assert_eq!(parse_compile_and_output("(sin 0)"), "return sin(0)\n");
  assert_eq!(parse_compile_and_output("(sinh 0)"), "return sinh(0)\n");
  assert_eq!(parse_compile_and_output("(tan 0)"), "return tan(0)\n");
  assert_eq!(parse_compile_and_output("(tanh 0)"), "return tanh(0)\n");
  assert_eq!(parse_compile_and_output("(ceil 0)"), "return ceil(0)\n");
  assert_eq!(parse_compile_and_output("(exp 0)"), "return exp(0)\n");
  assert_eq!(parse_compile_and_output("(floor 0)"), "return floor(0)\n");
  assert_eq!(parse_compile_and_output("(sqrt 10)"), "return sqrt(10)\n");
  assert_eq!(parse_compile_and_output("(pow 10 2)"), "return pow(10, 2)\n");
  assert_eq!(parse_compile_and_output("(fmod 10 2)"), "return fmod(10, 2)\n");
  assert_eq!(parse_compile_and_output("(fposmod 10 2)"), "return fposmod(10, 2)\n");
  assert_eq!(parse_compile_and_output("(posmod 10 2)"), "return posmod(10, 2)\n");
  assert_eq!(parse_compile_and_output("(sign 10)"), "return sign(10)\n");
  assert_eq!(parse_compile_and_output("(is-nan 10)"), "return is_nan(10)\n");
  assert_eq!(parse_compile_and_output("(is-inf 10)"), "return is_inf(10)\n");
  assert_eq!(parse_compile_and_output("(is-equal-approx 10 10)"), "return is_equal_approx(10, 10)\n");
  assert_eq!(parse_compile_and_output("(is-zero-approx 10)"), "return is_zero_approx(10)\n");
  assert_eq!(parse_compile_and_output("(stepify 0.5 2)"), "return stepify(5e-1, 2)\n");
  assert_eq!(parse_compile_and_output("(step-decimals 0.5)"), "return step_decimals(5e-1)\n");
  assert_eq!(parse_compile_and_output("(deg2rad 60)"), "return deg2rad(60)\n");
  assert_eq!(parse_compile_and_output("(rad2deg PI)"), "return rad2deg(PI)\n");
  assert_eq!(parse_compile_and_output("(linear2db 0)"), "return linear2db(0)\n");
  assert_eq!(parse_compile_and_output("(db2linear 0)"), "return db2linear(0)\n");
  assert_eq!(parse_compile_and_output("(log 10)"), "return log(10)\n");
  assert_eq!(parse_compile_and_output("(round 10)"), "return round(10)\n");
  assert_eq!(parse_compile_and_output("(wrapf 10 20 15)"), "return wrapf(10, 20, 15)\n");
  assert_eq!(parse_compile_and_output("(wrapi 10 20 15)"), "return wrapi(10, 20, 15)\n");
  assert_eq!(parse_compile_and_output("(cartesian2polar 10 10)"), "return cartesian2polar(10, 10)\n");
  assert_eq!(parse_compile_and_output("(polar2cartesian 10 PI)"), "return polar2cartesian(10, PI)\n");
  assert_eq!(parse_compile_and_output("(nearest-po2 17)"), "return nearest_po2(17)\n");
}

#[test]
pub fn wrapped_list_functions_test() {
  assert_eq!(parse_compile_and_output("(len [1 2])"), "return len([1, 2])\n");
  assert_eq!(parse_compile_and_output("(len [])"), "return len([])\n");
}

#[test]
pub fn wrapped_error_functions_test() {
  assert_eq!(parse_compile_and_output("(push-error \"A\")"), "return push_error(\"A\")\n");
  assert_eq!(parse_compile_and_output("(push-warning \"A\")"), "return push_warning(\"A\")\n");
}

#[test]
pub fn wrapped_load_functions_test() {
  assert_eq!(parse_compile_and_output("(load \"A\")"), "return load(\"A\")\n");
}

#[test]
pub fn wrapped_range_functions_test() {
  assert_eq!(parse_compile_and_output("(inverse-lerp 0 10 6)"), "return inverse_lerp(0, 10, 6)\n");
  assert_eq!(parse_compile_and_output("(lerp 0 10 0.4)"), "return lerp(0, 10, 4e-1)\n");
  assert_eq!(parse_compile_and_output("(lerp-angle 0 10 0.4)"), "return lerp_angle(0, 10, 4e-1)\n");
  assert_eq!(parse_compile_and_output("(range-lerp 5 0 10 -10 -20)"), "return range_lerp(5, 0, 10, -10, -20)\n");
  assert_eq!(parse_compile_and_output("(move-toward 10 5 1)"), "return move_toward(10, 5, 1)\n");
  assert_eq!(parse_compile_and_output("(ease 0.7 2.0)"), "return ease(7e-1, 2e0)\n");
}

#[test]
pub fn wrapped_misc_functions_test() {
  assert_eq!(parse_compile_and_output("(hash \"A\")"), "return hash(\"A\")\n");
  assert_eq!(parse_compile_and_output("(get-stack)"), "return get_stack()\n");
  assert_eq!(parse_compile_and_output("(print-stack)"), "return print_stack()\n");
  assert_eq!(parse_compile_and_output("(is-instance-valid (Reference:new))"), "return is_instance_valid(Reference.new())\n");
  assert_eq!(parse_compile_and_output("(parse-json nil)"), "return parse_json(GDLisp.nil)\n");
  assert_eq!(parse_compile_and_output("(to-json \"{}\")"), "return to_json(\"{}\")\n");
  assert_eq!(parse_compile_and_output("(validate-json \"{}\")"), "return validate_json(\"{}\")\n");
  assert_eq!(parse_compile_and_output("(dict2inst {})"), "return dict2inst({})\n");
  assert_eq!(parse_compile_and_output("(inst2dict (Reference:new))"), "return inst2dict(Reference.new())\n");
  assert_eq!(parse_compile_and_output("(str2var \"{}\")"), "return str2var(\"{}\")\n");
  assert_eq!(parse_compile_and_output("(var2str (Reference:new))"), "return var2str(Reference.new())\n");
  assert_eq!(parse_compile_and_output("(weakref (Reference:new))"), "return weakref(Reference.new())\n");
  assert_eq!(parse_compile_and_output("(funcref (Reference:new) \"potato\")"), "return funcref(Reference.new(), \"potato\")\n");
}

#[test]
pub fn convert_function_test() {
  assert_eq!(parse_compile_and_output("(convert 1 Int)"), "return GDLisp.convert(1, GDLisp.Int)\n");
  assert_eq!(parse_compile_and_output("(convert 1 TYPE_INT)"), "return GDLisp.convert(1, TYPE_INT)\n");
}

#[test]
pub fn convert_function_run_test() {
  assert_eq!(parse_and_run("((print (convert 1.5 Int)))"), "\n1\n");
  assert_eq!(parse_and_run("((print (convert 1.5 TYPE_INT)))"), "\n1\n");
}

#[test]
pub fn instance_from_id_function_test() {
  assert_eq!(parse_compile_and_output("(instance-from-id 9)"), "return instance_from_id(9)\n");
}

#[test]
pub fn instance_from_id_function_run_test() {
  assert_eq!(parse_and_run(r#"((defclass Foo ()
                                 (defvar foo "mystring"))
                               (let* ((x (Foo:new))
                                      (id (x:get-instance-id)))
                                 (print (instance-from-id id):foo)))"#),
             "\nmystring\n");
}

// TODO Test gensym at runtime once we can pretty-print symbols
