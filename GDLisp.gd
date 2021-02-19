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

class Symbol:
    # Note: This will be obsolete once we have StringName in GDScript,
    # which seems to be coming in Godot 4. For now, this manual
    # wrapper stores symbols in the least efficient way possible.
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
    if args is Cons:
        var result = args.car
        args = args.cdr
        while args is Cons:
            result += args.car
            args = args.cdr
        return result
    else:
        return 0

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
        var result = x
        while args is Cons:
            result /= float(args.car)
            args = args.cdr
        return result

func intdiv(x, args):
    if not (args is Cons):
        return 1 / int(x)
    else:
        var result = x
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

func not_(x):
    return not x

func list(args):
    return args

func yield_(x, y):
    if x is NilClass:
        return yield()
    else:
        return yield(x, y)

func vector(x, y, z):
    if z is NilClass:
        return Vector2(x, y)
    else:
        return Vector3(x, y, z)

func list_to_array(list):
    var arr = []
    while list is Cons:
        arr.push_back(list.car)
        list = list.cdr
    return arr

func array_to_list(arr):
    var outer = Cons.new(Nil, Nil)
    var curr = outer
    for elem in arr:
        curr.cdr = Cons.new(elem, Nil)
        curr = curr.cdr
    return outer.cdr

func elt(arr, n):
    return arr[n]
