
The GDLisp REPL
===============

Let's jump right in and take a look at some GDLisp code. Once you've
:ref:`built GDLisp <building>`, you should have an executable called
``gdlisp``. If you invoke this executable with no arguments, you'll be
placed in a read-eval-print loop, or REPL for short. In this
interactive interpreter, you can type GDLisp expressions or
declarations and see the results live. This interpreter is backed by a
running Godot instance, so you also have access to the full breadth of
built-in Godot types and functions, like ``Node`` and ``sqrt``.

Hello, world!
-------------

Let's start with everybody's favorite program.

::

   (print "Hello, world!")

If you type this into the REPL and hit enter, you'll see "Hello,
world!" printed to the screen. This calls the built-in function
``print`` with one string argument, namely the string "Hello, world!".
Congratulations! You've just written your first line of GDLisp.

If you just ran this, you'll notice that, in addition to the string,
the interpreter also printed out a stray set of parentheses ``()``.
This is normal and is for good reasons. In GDLisp, every expression
returns a value, even those like ``print`` that are usually only
called for their side effects. Since ``print`` has no meaningful
return value, it returns the nil value as a default, which is written
as ``()``. So when you see ``()``, you can read that as "nil".

.. Note:: You might wonder why the null value prints out as ``()``,
          rather than as the word "nil". This is because ``()`` is
          used as a placeholder for the empty list and is used as the
          final cdr cell in a proper list. You can read more about how
          lists are actually stored in GDLisp at :ref:`parser`.

Let's do some math.

::

   (+ 1 1)

This will, naturally, add one and one together and return the result:
two. Operators like ``+`` and ``-`` are written in prefix notation in
GDLisp, in contrast to GDScript and most non-Lisp languages, which use
infix notation. In fact, ``+`` isn't an operator at all; it's just a
function like ``print``. GDLisp is very liberal in what it considers a
valid function name, and ``+`` is a perfectly valid identifier.

The ``+`` function also supports variable arguments. The following
will add all four numbers together and print the total::

   (+ 1 2 3 4)

Since everything is written as prefix function calls, there's never
any ambiguity about operator precedence.

::

   (* 3 (+ 1 1) (+ 1 10 (* 2 4)))

All of the common mathematical operators work as functions in exactly
the way you'd expect. ``+`` is for addition, ``*`` is for
multiplication, ``-`` is for subtraction, and ``/`` is for division.
All of the operators support variable arguments, with associativity to
the left when it matters (so, for instance ``(/ a b c)`` is equivalent
to ``(/ (/ a b) c)``, not ``(/ a (/ b c))``).

Conditionals
------------

