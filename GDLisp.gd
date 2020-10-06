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
    func _init():
        pass

onready var Nil = NilClass.new()
