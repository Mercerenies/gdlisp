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

class Cell:
    var contents
    func _init(contents):
        self.contents = contents

onready var Nil = NilClass.new()

func length(x):
    var result = 0
    while x is Cons:
        result += 1
        x = x.cdr
    return result

func funcall(f, args):
    if not (f is Function):
        push_error("Attempt to call non-function")
    return f.call_funcv(args)

func plus(args):
    var result = 0
    while args is Cons:
        result += args.car
        args = args.cdr
    return result

func times(args):
    var result = 1
    while args is Cons:
        result *= args.car
        args = args.cdr
    return result

func minus(x, args):
    if not (args is Cons):
        return - x
    else:
        var result = x
        while args is Cons:
            result -= args.car
            args = args.cdr
        return result

func div(x, args):
    if not (args is Cons):
        return 1.0 / float(x)
    else:
        var result = float(x)
        while args is Cons:
            result /= float(args.car)
            args = args.cdr
        return result

func intdiv(x, args):
    if not (args is Cons):
        return 1 / int(x)
    else:
        var result = int(x)
        while args is Cons:
            result /= int(args.car)
            args = args.cdr
        return result

func eq(x, args):
    while args is Cons:
        if not (x == args.car):
            return false
        x = args.car
        args = args.cdr
    return true

func lt(x, args):
    while args is Cons:
        if not (x < args.car):
            return false
        x = args.car
        args = args.cdr
    return true

func gt(x, args):
    while args is Cons:
        if not (x > args.car):
            return false
        x = args.car
        args = args.cdr
    return true

func le(x, args):
    while args is Cons:
        if not (x <= args.car):
            return false
        x = args.car
        args = args.cdr
    return true

func ge(x, args):
    while args is Cons:
        if not (x >= args.car):
            return false
        x = args.car
        args = args.cdr
    return true

func ne(x, args):
    var outer = Cons.new(x, args)
    while outer is Cons:
        var inner = outer.cdr
        while inner is Cons:
            # Double negation here is hilariously nontrivial, as not
            # (NaN != NaN) is true but NaN == NaN is false. Once
            # again, NaN is hilarious and ridiculous.
            if not (outer.car != inner.car):
                return false
            inner = inner.cdr
        outer = outer.cdr
    return true
