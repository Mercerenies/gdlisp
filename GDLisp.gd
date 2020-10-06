extends Node

# Library file for GDLisp. This file must be included as a singleton
# in any project which uses GDLisp compiled files.

class NilClass:
    func _iter_init(arg):
        return false
    func _iter_next(arg):
        return false
    func _iter_get(arg):
        return self

class Cons:
    var car
    var cdr
    func _init(car, cdr):
        self.car = car
        self.cdr = cdr
    func _iter_init(arg):
        arg[0] = self
        return true
    func _iter_next(arg):
        arg[0] = arg[0].cdr
        return (arg[0] is Cons)
    func _iter_get(arg):
        return arg.car

class Function:
    var __is_gdlisp_function = true
    var __gdlisp_required = 0
    var __gdlisp_optional = 0
    var __gdlisp_rest = true
    func _init():
        pass
    func call_func(args):
        push_error("Unimplemented function")
    func call_funcv(args):
        return call_func(args)

onready var Nil = NilClass.new()

func funcall(f, args):
    if not (f is Function):
        push_error("Attempt to call non-function")
    return f.call_funcv(args)
