
use gdlisp::compile::error::{GDError, GDErrorF};
use gdlisp::pipeline::error::PError;
use gdlisp::pipeline::source::SourceOffset;
use gdlisp::runner::version::{get_godot_version, Version};

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
pub fn array_test() {
  assert_eq!(parse_compile_and_output("(array 9 10)"), "return [9, 10]\n");
  assert_eq!(parse_compile_and_output("(array 9 10 11)"), "return [9, 10, 11]\n");
}

#[test]
pub fn array_syntax_test() {
  assert_eq!(parse_compile_and_output("[1 2 3 4]"), "return [1, 2, 3, 4]\n");
}

#[test]
pub fn dictionary_test() {
  assert_eq!(parse_compile_and_output("(dict 1 2 3 4)"), "return {1: 2, 3: 4}\n");
}

#[test]
pub fn dictionary_odd_args_test() {
  // Drops the last arg silently.
  assert_eq!(parse_compile_and_output("(dict 1 2 3)"), "return {1: 2}\n");
}

#[test]
pub fn dictionary_literal_test() {
  assert_eq!(parse_compile_and_output("{1 2 3 4}"), "return {1: 2, 3: 4}\n");
}

#[test]
pub fn yield_test() {
  assert_eq!(parse_compile_and_output("(yield)"), "return yield()\n");
  assert_eq!(parse_compile_and_output("(yield 1 2)"), "return yield(1, 2)\n");
}

#[test]
pub fn assert_test() {
  assert_eq!(parse_compile_and_output("(assert #t)"), "return assert(true)\n");
  assert_eq!(parse_compile_and_output("(assert #t \"a\")"), "return assert(true, \"a\")\n");
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
  assert_eq!(parse_compile_decl("((defn foo (x y) (sys/call-magic ADDITION) 9) (defn run () (foo 10 20)))"),
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
  assert_eq!(parse_compile_and_output("(sys/split 0:car):car:car:car"),
             r#"var _split = 0.car
return _split.car.car.car
"#);
}

#[test]
pub fn split_call_test_2() {
  assert_eq!(parse_compile_and_output("(sys/split (sys/split 0:car):car):car:car"),
             r#"var _split = 0.car
var _split_0 = _split.car
return _split_0.car.car
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
pub fn wrapped_error_functions_test() {
  assert_eq!(parse_compile_and_output("(push-error \"A\")"), "return push_error(\"A\")\n");
  assert_eq!(parse_compile_and_output("(push-warning \"A\")"), "return push_warning(\"A\")\n");
}

#[test]
pub fn wrapped_load_functions_test() {
  assert_eq!(parse_compile_and_output("(load \"A\")"), "return load(\"A\")\n");
}

#[test]
pub fn failed_preload_function_test() {
  assert_eq!(parse_compile_and_output_err("(preload \"A\")"),
             Err(PError::from(GDError::new(GDErrorF::BadPreloadArgument(String::from("A")), SourceOffset(0)))));
}

#[test]
pub fn wrapped_range_functions_test() {
  assert_eq!(parse_compile_and_output("(inverse-lerp 0 10 6)"), "return inverse_lerp(0, 10, 6)\n");
  assert_eq!(parse_compile_and_output("(lerp 0 10 0.4)"), "return lerp(0, 10, 4e-1)\n");
  assert_eq!(parse_compile_and_output("(lerp-angle 0 10 0.4)"), "return lerp_angle(0, 10, 4e-1)\n");
  assert_eq!(parse_compile_and_output("(range-lerp 5 0 10 -10 -20)"), "return range_lerp(5, 0, 10, -10, -20)\n");
  assert_eq!(parse_compile_and_output("(move-toward 10 5 1)"), "return move_toward(10, 5, 1)\n");
  assert_eq!(parse_compile_and_output("(ease 0.7 2.0)"), "return ease(7e-1, 2e0)\n");
  assert_eq!(parse_compile_and_output("(smoothstep 1.0 5.0 0.5)"), "return smoothstep(1e0, 5e0, 5e-1)\n");
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
  assert_eq!(parse_compile_and_output("(type-exists \"Reference\")"), "return type_exists(\"Reference\")\n");
  assert_eq!(parse_compile_and_output("(Color8 0 0 0)"), "return Color8(0, 0, 0)\n");
  assert_eq!(parse_compile_and_output("(Color8 0 0 0 255)"), "return Color8(0, 0, 0, 255)\n");
  assert_eq!(parse_compile_and_output("(ColorN \"red\")"), "return ColorN(\"red\")\n");
  assert_eq!(parse_compile_and_output("(ColorN \"red\" 1.0)"), "return ColorN(\"red\", 1e0)\n");
  assert_eq!(parse_compile_and_output("(var2bytes 0)"), "return var2bytes(0)\n");
  assert_eq!(parse_compile_and_output("(var2bytes 0 #f)"), "return var2bytes(0, false)\n");
  assert_eq!(parse_compile_and_output("(bytes2var ())"), "return bytes2var(null)\n");
  assert_eq!(parse_compile_and_output("(bytes2var () #f)"), "return bytes2var(null, false)\n");
}

#[test]
pub fn convert_function_test() {
  assert_eq!(parse_compile_and_output("(convert 1 Int)"), "return GDLisp._convert(1, GDLisp.Int)\n");
  assert_eq!(parse_compile_and_output("(convert 1 TYPE_INT)"), "return GDLisp._convert(1, TYPE_INT)\n");
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

#[test]
pub fn str_test() {
  assert_eq!(parse_compile_and_output("(str 3)"), "return str(3)\n");
  assert_eq!(parse_compile_and_output("(str 3 \"a\")"), "return str(3, \"a\")\n");
}

#[test]
pub fn printerr_test() {
  assert_eq!(parse_compile_and_output("(printerr 3 \"a\")"), "return printerr(3, \"a\")\n");
}

#[test]
pub fn print_test() {
  assert_eq!(parse_compile_and_output("(print 3 \"a\")"), "return print(3, \"a\")\n");
}

#[test]
pub fn printt_test() {
  assert_eq!(parse_compile_and_output("(printt 3 \"a\")"), "return printt(3, \"a\")\n");
}

#[test]
pub fn prints_test() {
  assert_eq!(parse_compile_and_output("(prints 3 \"a\")"), "return prints(3, \"a\")\n");
}

#[test]
pub fn printraw_test() {
  assert_eq!(parse_compile_and_output("(printraw 3 \"a\")"), "return printraw(3, \"a\")\n");
}

#[test]
pub fn print_debug_test() {
  assert_eq!(parse_compile_and_output("(print-debug 3 \"a\")"), "return print_debug(3, \"a\")\n");
}

#[test]
pub fn builtin_constant_on_type_test() {
  assert_eq!(parse_compile_and_output("(print Transform2D:IDENTITY)"), "return print(GDLisp._Transform2D.IDENTITY)\n");
}

#[test]
pub fn builtin_type_constructor_test() {
  assert_eq!(parse_compile_and_output("(Bool 0)"), "return bool(0)\n");
  assert_eq!(parse_compile_and_output("(Int 99.1)"), "return int(9.91e1)\n");
  assert_eq!(parse_compile_and_output("(Float \"0\")"), "return float(\"0\")\n");
  assert_eq!(parse_compile_and_output("(String 10)"), "return String(10)\n");
  assert_eq!(parse_compile_and_output("(Rect2 V{0 0} V{1 1})"), "return Rect2(Vector2(0, 0), Vector2(1, 1))\n");
  assert_eq!(parse_compile_and_output("(Rect2 0 0 1 1)"), "return Rect2(0, 0, 1, 1)\n");
  assert_eq!(parse_compile_and_output("(AABB V{0 0 0} V{1 1 1})"), "return AABB(Vector3(0, 0, 0), Vector3(1, 1, 1))\n");
  assert_eq!(parse_compile_and_output("(RID (Reference:new))"), "return RID(Reference.new())\n");
  assert_eq!(parse_compile_and_output("(Dictionary {})"), "return Dictionary({})\n");
  assert_eq!(parse_compile_and_output("(Array [])"), "return Array([])\n");
  assert_eq!(parse_compile_and_output("(PoolColorArray [])"), "return PoolColorArray([])\n");
  assert_eq!(parse_compile_and_output("(PoolByteArray [])"), "return PoolByteArray([])\n");
  assert_eq!(parse_compile_and_output("(PoolIntArray [])"), "return PoolIntArray([])\n");
  assert_eq!(parse_compile_and_output("(PoolRealArray [])"), "return PoolRealArray([])\n");
  assert_eq!(parse_compile_and_output("(PoolVector2Array [])"), "return PoolVector2Array([])\n");
  assert_eq!(parse_compile_and_output("(PoolVector3Array [])"), "return PoolVector3Array([])\n");
  assert_eq!(parse_compile_and_output("(Vector2 1 2)"), "return Vector2(1, 2)\n");
  assert_eq!(parse_compile_and_output("(Vector3 0 1 2)"), "return Vector3(0, 1, 2)\n");
  assert_eq!(parse_compile_and_output("(Transform2D ())"), "return Transform2D(null)\n");
  assert_eq!(parse_compile_and_output("(Transform2D V{0 0} V{0 0} V{0 0})"), "return Transform2D(Vector2(0, 0), Vector2(0, 0), Vector2(0, 0))\n");
  assert_eq!(parse_compile_and_output("(Transform2D 0 V{1 1})"), "return Transform2D(0, Vector2(1, 1))\n");
  assert_eq!(parse_compile_and_output("(Plane V{0 0 0} 10)"), "return Plane(Vector3(0, 0, 0), 10)\n");
  assert_eq!(parse_compile_and_output("(Plane V{0 0 0} V{0 0 1} V{1 0 0})"), "return Plane(Vector3(0, 0, 0), Vector3(0, 0, 1), Vector3(1, 0, 0))\n");
  assert_eq!(parse_compile_and_output("(Plane 0 0 0 1)"), "return Plane(0, 0, 0, 1)\n");
  assert_eq!(parse_compile_and_output("(Quat ())"), "return Quat(null)\n");
  assert_eq!(parse_compile_and_output("(Quat V{0 0 0} 1)"), "return Quat(Vector3(0, 0, 0), 1)\n");
  assert_eq!(parse_compile_and_output("(Quat 0 0 0 1)"), "return Quat(0, 0, 0, 1)\n");
  assert_eq!(parse_compile_and_output("(Basis ())"), "return Basis(null)\n");
  assert_eq!(parse_compile_and_output("(Basis V{0 0 0})"), "return Basis(Vector3(0, 0, 0))\n");
  assert_eq!(parse_compile_and_output("(Basis V{0 0 0} 1)"), "return Basis(Vector3(0, 0, 0), 1)\n");
  assert_eq!(parse_compile_and_output("(Basis V{0 0 0} V{0 0 1} V{1 0 0})"), "return Basis(Vector3(0, 0, 0), Vector3(0, 0, 1), Vector3(1, 0, 0))\n");
  assert_eq!(parse_compile_and_output("(Transform ())"), "return Transform(null)\n");
  assert_eq!(parse_compile_and_output("(Transform () V{0 0 0})"), "return Transform(null, Vector3(0, 0, 0))\n");
  assert_eq!(parse_compile_and_output("(Transform V{0 0 0} V{0 0 1} V{1 0 0} V{2 2 2})"), "return Transform(Vector3(0, 0, 0), Vector3(0, 0, 1), Vector3(1, 0, 0), Vector3(2, 2, 2))\n");
  assert_eq!(parse_compile_and_output("(Color 0)"), "return Color(0)\n");
  assert_eq!(parse_compile_and_output("(Color 0 0 0)"), "return Color(0, 0, 0)\n");
  assert_eq!(parse_compile_and_output("(Color 0 0 0 1)"), "return Color(0, 0, 0, 1)\n");

}

#[test]
pub fn nodepath_constructor_test() {
  assert_eq!(parse_compile_and_output(r#"(let ((x "A")) (NodePath x))"#), r#"var x = "A"
return GDLisp._NodePath(x)
"#);
  assert_eq!(parse_compile_and_output(r#"(NodePath 0)"#), "return GDLisp._NodePath(0)\n");
  assert_eq!(parse_compile_and_output(r#"(NodePath "a")"#), "return @\"a\"\n");
}

#[test]
pub fn str_running_test() {
  assert_eq!(parse_and_run("((print (str 1 2 #t)))"), "\n12True\n");
}

#[test]
pub fn str_running_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall #'str 1 2 #t)))"), "\n12True\n");
}

#[test]
pub fn printerr_running_test() {
  let StringOutput { stdout, stderr } = parse_and_run_with_stderr("((printerr \"printerr_running_test OUTPUT\"))");
  assert_eq!(stdout, "\n");
  assert!(stderr.contains("printerr_running_test OUTPUT"));
}

#[test]
pub fn printerr_running_test_indirect() {
  let StringOutput { stdout, stderr } = parse_and_run_with_stderr("((funcall #'printerr \"printerr_running_test_indirect OUTPUT\"))");
  assert_eq!(stdout, "\n");
  assert!(stderr.contains("printerr_running_test_indirect OUTPUT"));
}

#[test]
pub fn printraw_running_test() {
  assert_eq!(parse_and_run("((printraw 1 2 #t))"), "\n12True"); // Note: No \n at end
}

#[test]
pub fn printraw_running_test_indirect() {
  assert_eq!(parse_and_run("((funcall #'printraw 1 2 #t))"), "\n12True"); // Note: No \n at end
}

#[test]
pub fn print_running_test() {
  assert_eq!(parse_and_run("((print 1 2 #t))"), "\n12True\n");
}

#[test]
pub fn range_running_test() {
  assert_eq!(parse_and_run("((print (range 5)) (print (range 1 5)) (print (range 1 5 2)) (print (range 5 1 -1)))"),
             "\n[0, 1, 2, 3, 4]\n[1, 2, 3, 4]\n[1, 3]\n[5, 4, 3, 2]\n");
}

#[test]
pub fn range_running_test_indirect() {
  assert_eq!(parse_and_run("((print (funcall #'range 5)) (print (funcall #'range 1 5)) (print (funcall #'range 1 5 2)) (print (funcall #'range 5 1 -1)))"),
             "\n[0, 1, 2, 3, 4]\n[1, 2, 3, 4]\n[1, 3]\n[5, 4, 3, 2]\n");
}

#[test]
pub fn print_running_test_indirect() {
  assert_eq!(parse_and_run("((funcall #'print 1 2 #t))"), "\n12True\n");
}

#[test]
pub fn deep_equal_test() {
  // deep_equal is only available in Godot 3.5 and later, so we need
  // to disable this test if we're running on an older version.
  let godot_version = get_godot_version().unwrap().version;
  if godot_version >= Version::new(3, 5, 0) {
    assert_eq!(parse_and_run("((print (deep-equal {\"a\" 1} {\"a\" 1})))"), "\nTrue\n");
  }
}

#[test]
pub fn cons_accessor_test_1() {
  assert_eq!(parse_and_run("((print (quote (1 . 2)):car))"), "\n1\n");
  assert_eq!(parse_and_run("((print (quote (1 . 2)):cdr))"), "\n2\n");
}

#[test]
pub fn cons_accessor_test_2() {
  assert_eq!(parse_and_run("((print (quote ((1 . 2) . (3 . 4))):caar))"), "\n1\n");
  assert_eq!(parse_and_run("((print (quote ((1 . 2) . (3 . 4))):cadr))"), "\n2\n");
  assert_eq!(parse_and_run("((print (quote ((1 . 2) . (3 . 4))):cdar))"), "\n3\n");
  assert_eq!(parse_and_run("((print (quote ((1 . 2) . (3 . 4))):cddr))"), "\n4\n");
}

#[test]
pub fn cons_accessor_test_3() {
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):caaar))"), "\n1\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):caadr))"), "\n2\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):cadar))"), "\n3\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):caddr))"), "\n4\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):cdaar))"), "\n5\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):cdadr))"), "\n6\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):cddar))"), "\n7\n");
  assert_eq!(parse_and_run("((print (quote (((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))):cdddr))"), "\n8\n");
}

#[test]
pub fn nodepath_running_test() {
  assert_eq!(parse_and_run("((let ((x (NodePath (let ((y \"foo/bar\")) y)))) (print (x:get-name 0)) (print (x:get-name 1))))"), "\nfoo\nbar\n");
  assert_eq!(parse_and_run("((let ((x (NodePath \"foo/bar\"))) (print (x:get-name 0)) (print (x:get-name 1))))"), "\nfoo\nbar\n");
}

#[test]
pub fn vector_constant_print_test() {
  assert_eq!(parse_and_run("((print Vector2:LEFT) (print Vector3:AXIS_Z))"), "\n(-1, 0)\n2\n");
}

#[test]
pub fn object_constant_value_test() {
  // We had to hard-code these values in, so validate that they match
  // what Godot expects.
  assert_eq!(parse_and_run(r#"((print (= CONNECT_DEFERRED ConnectFlags:DEFERRED))
                               (print (= CONNECT_PERSIST ConnectFlags:PERSIST))
                               (print (= CONNECT_ONESHOT ConnectFlags:ONESHOT))
                               (print (= CONNECT_REFERENCE_COUNTED ConnectFlags:REFERENCE_COUNTED))
                               (print (= NOTIFICATION_POSTINITIALIZE Notification:POSTINITIALIZE))
                               (print (= NOTIFICATION_PREDELETE Notification:PREDELETE)))"#),
             "\nTrue\nTrue\nTrue\nTrue\nTrue\nTrue\n");
}

#[test]
pub fn thread_macro_test() {
  assert_eq!(parse_compile_and_output("(-> 1 foo1 (foo1) (foo2 2))"), "return foo2(foo1(foo1(1)), 2)\n");
  assert_eq!(parse_compile_and_output("(-> 1)"), "return 1\n");
}

#[test]
pub fn last_thread_macro_test() {
  assert_eq!(parse_compile_and_output("(->> 1 foo1 (foo1) (foo2 2))"), "return foo2(2, foo1(foo1(1)))\n");
  assert_eq!(parse_compile_and_output("(->> 1)"), "return 1\n");
}

#[test]
pub fn as_thread_macro_test_1() {
  assert_eq!(parse_compile_and_output("(as-> 1 v)"),
             "return 1\n");
}

#[test]
pub fn as_thread_macro_test_2() {
  assert_eq!(parse_compile_and_output("(as-> 1 v (foo1 v) (foo2 v 2) (foo2 2 v))"),
             r#"var v = 1
var v_0 = foo1(v)
var v_1 = foo2(v_0, 2)
return foo2(2, v_1)
"#);
}

#[test]
pub fn as_thread_macro_test_3() {
  assert_eq!(parse_compile_and_output("(as-> 1 v (foo1 v) (foo2 v v) (foo2 2 v))"),
             r#"var v = 1
var v_0 = foo1(v)
var v_1 = foo2(v_0, v_0)
return foo2(2, v_1)
"#);
}

#[test]
pub fn list_is_not_shared_running_test() {
  assert_eq!(parse_and_run(r#"
    ((let* ((first-list (list 1 2 3 4))
            (second-list (apply #'list first-list)))
       (set first-list:car 0)
       (print (list->array first-list))
       (print (list->array second-list))))"#),
             "\n[0, 2, 3, 4]\n[1, 2, 3, 4]\n");
}

#[test]
pub fn array_find_test() {
  assert_eq!(parse_and_run(r#"
    ((let ((arr [1 2 3 4 5 6]))
       (print (array/find (lambda (x) (> x 3)) arr))
       (print (array/find (lambda (x) (> x 10)) arr))))"#),
             "\n4\nNull\n");
}

#[test]
pub fn list_find_test() {
  assert_eq!(parse_and_run(r#"
    ((let ((list '(1 2 3 4 5 6)))
       (print (list/find (lambda (x) (> x 3)) list))
       (print (list/find (lambda (x) (> x 10)) list))))"#),
             "\n4\nNull\n");
}

#[test]
pub fn dict_find_test() {
  assert_eq!(parse_and_run(r#"
    ((let ((dict {1 2 3 4 5 6}))
       (print (dict/find (lambda (k v) (> k 1)) dict))
       (print (dict/find (lambda (k v) (> v 1)) dict))
       (print (dict/find (lambda (k v) (> k 100)) dict))))"#),
             "\n3\n1\nNull\n");
}

#[test]
pub fn vector_map_test() {
  assert_eq!(parse_and_run("((let ((a V{1 3})) (print (vector/map (lambda (x) (+ x 1)) a))))"),
             "\n(2, 4)\n");
  assert_eq!(parse_and_run("((let ((a V{1 3 10})) (print (vector/map (lambda (x) (+ x 2)) a))))"),
             "\n(3, 5, 12)\n");
}

#[test]
pub fn vector_map_two_arguments_test() {
  assert_eq!(parse_and_run("((let ((a V{1 3}) (b V{10 30})) (print (vector/map #'+ a b))))"),
             "\n(11, 33)\n");
  assert_eq!(parse_and_run("((let ((a V{1 3 10}) (b V{9 8 7})) (print (vector/map #'+ a b))))"),
             "\n(10, 11, 17)\n");
}

// TODO Test gensym at runtime once we can pretty-print symbols
