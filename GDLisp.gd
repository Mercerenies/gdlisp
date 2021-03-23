extends Node

# Library file for GDLisp. This file must be included as a singleton
# in any project which uses GDLisp compiled files.

class Cons:
    var car
    var cdr
    func _init(car, cdr):
        self.car = car
        self.cdr = cdr

class Function:
    var __is_gdlisp_function = true
    var __gdlisp_required = 0
    var __gdlisp_optional = 0
    var __gdlisp_rest = 1
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

enum Mouse {
    LEFT = BUTTON_LEFT,
    RIGHT = BUTTON_RIGHT,
    MIDDLE = BUTTON_MIDDLE,
    XBUTTON1 = BUTTON_XBUTTON1,
    XBUTTON2 = BUTTON_XBUTTON2,
    WHEEL_UP = BUTTON_WHEEL_UP,
    WHEEL_DOWN = BUTTON_WHEEL_DOWN,
    WHEEL_LEFT = BUTTON_WHEEL_LEFT,
    WHEEL_RIGHT = BUTTON_WHEEL_RIGHT,
    MASK_LEFT = BUTTON_MASK_LEFT,
    MASK_RIGHT = BUTTON_MASK_RIGHT,
    MASK_MIDDLE = BUTTON_MASK_MIDDLE,
    MASK_XBUTTON1 = BUTTON_MASK_XBUTTON1,
    MASK_XBUTTON2 = BUTTON_MASK_XBUTTON2,
}

enum Margin {
    LEFT = MARGIN_LEFT,
    TOP = MARGIN_TOP,
    RIGHT = MARGIN_RIGHT,
    BOTTOM = MARGIN_BOTTOM,
}

enum Corner {
    TOP_LEFT = CORNER_TOP_LEFT,
    TOP_RIGHT = CORNER_TOP_RIGHT,
    BOTTOM_RIGHT = CORNER_BOTTOM_RIGHT,
    BOTTOM_LEFT = CORNER_BOTTOM_LEFT,
}

enum Orientation {
    VERTICAL = VERTICAL,
    HORIZONTAL = HORIZONTAL,
}

enum HAlign {
    LEFT = HALIGN_LEFT,
    CENTER = HALIGN_CENTER,
    RIGHT = HALIGN_RIGHT,
}

enum VAlign {
    TOP = VALIGN_TOP,
    CENTER = VALIGN_CENTER,
    BOTTOM = VALIGN_BOTTOM,
}

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

func mod(x, y):
    return x % y

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

func vector(x, y, z):
    if z == null:
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
    var outer = Cons.new(null, null)
    var curr = outer
    for elem in arr:
        curr.cdr = Cons.new(elem, null)
        curr = curr.cdr
    return outer.cdr

func elt(arr, n):
    return arr[n]

func istype(value, type):
    if typeof(type) == TYPE_INT:
        return typeof(value) == type
    else:
        return value is type
