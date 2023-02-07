
Introduction to Lisp
====================

The term "Lisp" encompasses a broad collection of programming
languages dating back to the 1960s. This section aims to describe the
Lisp ecosystem in a broad sense and to talk about the parts of GDLisp
that are like other Lisp dialects, without going into too much detail
on the Godot-specific parts. If you've used another Lisp dialect in
the past, you can likely skim this section or skip it entirely.

What is Lisp?
-------------

Lisp dialects are homoiconic, functional programming languages with a
distinctive syntax. If you've seen Lisp code in the past, you'll
probably recognize it offhand.

::

    (let ((x 1) (y 1))
      (if (> x 0) (+ x y) y))

This is a GDLisp expression. It declares two local variables: ``x``
and ``y``. Both variables get the initial value of 1. Then we check
whether or not ``x`` is greater than zero. If so, we add ``x`` and
``y`` together, and if not, we simply return ``y``.

Aside from some minor syntax sugar, every piece of GDLisp syntax is
shown in the above snippet of code. Specifically, a GDLisp source file
is made up of zero or more *S-expressions*. An S-expression, or
symbolic expression, is defined recursively to be either a *literal*
(such as a number, a symbol, or a string) or a list of zero or more
S-expressions wrapped in parentheses.

.. Note:: This is a slight oversimplification. Strictly speaking, an
          S-expression is either a literal or a *cons cell*, which may
          or may not form a proper list, but in practice most cons
          cells written in a GDLisp program are proper lists. For a
          full description of the syntax of GDLisp, see :ref:`parser`.

For example, ``3`` is an S-expression representing, naturally, the
number 3. ``(10 20)`` is an S-expression which is a list. That list
contains two literals: 10 and 20. Lists can be nested, such as ``((9)
8)``. This S-expression is a list of two elements. The first element
is *itself* a list of one element, while the second is a literal.

This is the entirety of the GDLisp syntax. Every construct in GDLisp
can be built up from these basic rules. There is some syntax sugar on
top of these constructs to make common idioms (such as calling methods
on objects, or getting Godot nodes by name) easier.

This raises the natural question: What do we *do* with this syntax?
Every GDLisp expression is read in the same way. Strings and numbers
evaluate to themselves, so ``3``, when evaluated, returns the
numerical value 3. Symbols, which are unquoted barewords, evaluate to
the value of the variable with the given name. In the ``let`` snippet
above, ``x`` and ``y`` are symbols, and inside the ``let`` block, they
evaluate to the current value of the variables ``x`` and ``y``,
respectively. Finally, lists can act as function calls, macro calls,
or special forms. We'll dive more into the specifics of those later
on, but the basic idea is that an S-expression which is a list takes
the form

::

   (name arg1 arg2 ...)

where ``name`` is usually a symbol. The function or other form with
the given name will be *called* with the given arguments.

Why Lisp?
---------

So why should we care? Our syntax has been reduced to two simple
rules, in contrast to languages like Python which require complex
parsers to even understand the language. This gives us some powerful
introspection capabilities, and none more so than *macros*.

If you've been programming in GDScript or another programming language
for very long, you've likely encountered *functions*. When we write
``example(a, b, c)`` in GDScript, what happens is this: Godot invokes
a function called ``example`` with the arguments ``a``, ``b``, and
``c``. But that's not all that happens. First, ``a``, ``b``, and ``c``
have to be evaluated. They might be variables, or function calls of
their own, or something more complicated.

The same is true in GDLisp, with some caveats. If there exists a
function called ``example`` in the current scope, then the above
GDScript call is equivalent to ``(example a b c)``. Remember that this
is an S-expression of length 4. The first element indicates the
function's name and the other three are its arguments.

However, GDLisp also supports *macros*. A macro is like a function,
but whereas a function takes runtime values and returns a value,
macros operate on code. That is, macros take input in the form of
source code and produce new code. Calling a macro is done in the same
way as calling a function, so if ``example`` happened to be a *macro*
in the current scope, then ``(example a b c)`` would invoke the macro
with three arguments. However, those three arguments would *not* be
evaluated. Instead, they would literally be passed, as S-expressions
representing pieces of syntax, to ``example``. ``example`` then
returns a new S-expression that will be evaluated in place of the
original macro call.

Let's see a more concrete example. GDLisp already provides a macro
called ``and``, but let's pretend for the moment that it does not.
Let's write a function called ``and`` that takes two Boolean arguments
and returns true if and only if both arguments are true.

::

   (defn and (a b)
     (if a
       b
       #f))

Let's break this down for a moment. ``defn`` is used to declare
functions, so this defines a *function* (not a macro) called ``and``.
The function takes two arguments ``a`` and ``b``. Inside the function,
if ``a`` is true, then we return ``b``, and if ``a`` is false, then we
return false (represented by the literal ``#f``). Note that we never
actually use the word ``return`` here. In GDLisp, the last expression
of a function is automatically returned.

This function has a problem, though. In most programming languages,
the Boolean operators ``and`` and ``or`` exhibit `Short-circuit
evaluation <https://en.wikipedia.org/wiki/Short-circuit_evaluation>`_.
That is, if ``a`` happens to be false, then the whole expression is
false, regardless of the value of ``b``, so there's no sense in even
evaluating the latter. But our current function doesn't do that. By
definition, functions evaluate all of their arguments before even
trying to execute.

What we want is a special sort of function-like object that *doesn't*
evaluate its arguments and instead produces some code that does what
we want. And that's exactly what a macro is. Let's rewrite this
function to be a macro instead.

::

   (defmacro and (a b)
     `(if ,a
        ,b
        #f))

That's it. That's the macro. We've changed ``defn`` to ``defmacro`` to
indicate our intent to write a macro, and we've sprinkled some funny
commas and backticks in the code.

Now our macro takes two unevaluated arguments ``a`` and ``b``, and it
produces an ``if`` statement. The backtick starts a :ref:`quasiquote
<quoting>`, which is a sort of fancy template. The quasiquote is
interpreted literally, without evaluating anything inside of it, until
we encounter an *unquote* expression, indicated by the comma, which
interpolates a value into the resulting expression. This is sort of
like string interpolation in Python or Javascript, but it works for
arbitrary S-expressions, not just strings.

.. Note:: We're not introducing new fundamental syntax here. We're
          just introducing some syntax sugar. The backtick and comma
          expand to, respectively, the words ``quasiquote`` and
          ``unquote``. So, written without any syntax sugar, the macro
          declaration above is equivalent to::

            (defmacro and (a b)
              (quasiquote (if (unquote a)
                            (unquote b)
                            #f)))

          but the former is quite a bit more readable.

So the true benefit of Lisp is this. Since our code is merely data, we
can write functions which operate on code and produce more code, and
we write those functions in the same language that we would write
ordinary value-based functions as well. These "code functions" are
called macros. As we delve further into the GDLisp programming
language, we'll encounter several built-in macros. In fact, ``and`` is
a macro built-in to the standard library (though the implementation is
a bit more complex than our example here, as the standard library
macro accounts for arbitrary numbers of arguments).

Functional Programming
----------------------

Functional programming is a paradigm that puts emphasis on treating
functions as first-class citizens of the language and being able to
pass around functions and construct new functions from existing ones
using combinators. GDLisp is designed to support a functional style of
programming. GDScript, on its own, has some basic facilities for
functional programming, but they can be obtuse. For example, `FuncRef
<https://docs.godotengine.org/en/stable/classes/class_funcref.html>`_
can be used to wrap an object and a method name into a callable
object. However, FuncRef cannot create closures around local
variables. Likewise, there's no way to declare a local function that's
only needed for a few lines.

In GDLisp, all of these restrictions are lifted. Functions are
first-class values in the language. To convert a function into a value
that can be assigned to a variable, the ``function`` primitive can be
used.

::

   (defn addone (a)
     (+ a 1))

   (function addone)

Here, ``addone`` is a function that can be called. However, being a
function, it can't very well be passed to another function as an
argument. But by wrapping the name in the ``(function ...)`` special
form, we convert it into a callable object. This is a common enough
operation that it, like quasiquoting above, has a shortcut syntax.
Namely, ``#'addone`` is equivalent to ``(function addone)``. To call a
function that has been wrapped in this way, we use
:ref:`function-funcall`.

::

   (funcall (function addone) 10) ; Returns 11

But that's not all. GDLisp also fully supports *closures*. The
``lambda`` primitive constructs a local function which has full access
to its enclosing scope, including any local variables, as well as the
current ``self`` object.

Suppose, as a random example, that we have an array of numbers and we
want to add some constant to each element of that array, producing a
new array of values. In GDScript

.. code-block:: gdscript

   var old_array = [1, 2, 3, 4, 5]
   var my_constant = 10
   var new_array = []
   for x in old_array:
       new_array.push_back(x + my_constant)
   return new_array

If we find ourselves doing this sort of transformation a lot, we might
wish to capture the behavior in a "for-each" sort of function that
applies a FuncRef to each element of a list.

.. code-block:: gdscript

   func map(old_array, f):
       var new_array = []
       for x in old_array:
           new_array.push_back(f.call_func(x))
       return new_array

But we can't really easily apply this to our "add a constant"
situation, since FuncRef can't easily capture additional values like
``my_constant``. We can create a sort of FuncRef-like object that
*does* capture these values and use that instead.

.. code-block:: gdscript

   class AddConstant:
       var constant

       func _init(c):
           constant = c

       func call_func(arg):
           return arg + constant

    var old_array = [1, 2, 3, 4, 5]
    var my_constant = 10
    return map(old_array, AddConstant.new(10))

But this is a lot of extra code to capture the intuitive notion of
"add a constant to a number".

In GDLisp, this transformation is already built-in and is called
:ref:`function-array-map`. And if we want to use a local variable, we
can simply create a ``lambda`` that automatically captures that local
variable.

::

   (let ((old-array [1 2 3 4 5])
         (my-constant 10))
     (array/map (lambda (x) (+ x my-constant)) old-array))

``lambda`` constructs a local function that, when called, adds
``my-constant`` to the argument. Then ``array/map`` applies that to
each element of our array.

Functional programming is all about breaking a problem down into
manageable pieces. We've taken the complex problem of "add some number
to each element of an array" and broken it down into two easy pieces:
"do something to each element of an array" and "add a constant to a
number". The first piece was built-in under the name ``array/map``,
and the second was a simple ``lambda`` expression.
