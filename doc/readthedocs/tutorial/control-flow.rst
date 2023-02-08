
Basic Control Flow
==================

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

Comments
--------

GDLisp line comments start with a semicolon.

::

   ; This is a line comment

By convention, a single semicolon is used to annotate a line of code,
and a pair of semicolons is used when a line comment stands on its own
line.

::

   ;; This is a standalone line comment.
   (+ 1 1) ; This comment annotates other code.

Block comments begin with ``#|`` and are terminated by ``|#``. Block
comments cannot be nested.

Data Types
----------

GDLisp supports the same basic data types as GDScript, albeit with
different syntax.

The GDLisp equivalent of Godot's "null" value is called "nil" and is
written in GDLisp as ``()``. GDLisp also defines a global constant
called ``nil``, which evaluates to ``()``.

The Boolean values "true" and "false" are written, respectively, as
``#t`` and ``#f``.

Integers and floats are written in GDLisp in the same way as in
GDScript.

Strings are written in GDLisp using double quotes ``"..."``. Strings
*cannot* be written using single quotes, as the single quote character
is used for something different in GDLisp. Aside from that, all of the
usual escape sequences work. Additionally, GDLisp supports two methods
of escaping Unicode values in strings. The first consists of ``\u``
followed by exactly four hexadecimal digits, exactly like GDScript's
syntax for the same. The second consists of ``\u{...}``, with any
number of hexadecimal digits enclosed in the curly braces. This syntax
can be used to represent *any* Unicode code point, including those
outside the basic multilingual plane.

Conditionals
------------

If Expressions
^^^^^^^^^^^^^^

The most basic conditional in GDLisp is the :ref:`macro-if` macro.

::

   (if some-conditional true-case false-case)

``if`` is a macro that takes three arguments. The first argument is
the conditional, which is evaluated according to Godot's rules of
truthiness. That is, zero, false, the empty string, and other
intuitively "empty" objects are considered false.

::

   (if (> x 0)
     (print "x is positive!")
     (print "x is not positive!"))

This expression tests whether the variable ``x`` is positive and, in
either case, prints out an appropriate message. Note that ``>``, like
``+``, is a perfectly valid identifier in GDLisp and is in fact a
simple, built-in function just like ``+``.

Up to this point, we've been calling these forms "expressions"
somewhat informally, but it's important to note that even control
forms like ``if`` are expressions in GDLisp, whereas in a language
like GDScript or Python ``if`` would be considered a statement. Put
more colloquially, ``if`` is really no different than any other
expression and can also return values. So we could've written our
``print`` like so.

::

   (print (if (> x 0) "x is positive!" "x is not positive!"))

Or we could have returned the resulting string from a function, or
assigned it to a variable, or any number of other things. GDLisp does
not distinguish between statements and expressions. Anything that can
be used in expression context returns a value.

The true and false branches of the ``if`` statement are each
individual arguments, which means each branch expects a single
expression. If you need to perform multiple actions inside one of the
branches, you can use the :ref:`expr-progn` special form.

::

    (if (> x 0)
      (progn (print "x is positive!")
             (print "Have a lovely day :)"))
      (print "x is not positive!"))

``progn`` is a special form that takes zero or more arguments,
evaluates each of them in order, and then returns the value of the
last one.

Finally, the "false" branch is optional and will default to the nil
expression ``()``.

::

   (if (> x 0)
      (progn (print "x is positive!")
             (print "Have a lovely day :)")))

Though if your intention is to execute some code conditionally for its
side effects, you might find the macros :ref:`macro-when` and
:ref:`macro-unless` more useful.

Cond Expressions
^^^^^^^^^^^^^^^^

Extending our ``if`` expression from above, we might consider adding
another case to distinguish between negative numbers and zero.

::

   (if (> x 0)
     (print "x is positive!")
     (if (< x 0)
       (print "x is negative!")
       (print "x is zero!")))

Nesting additional ``if`` branches in an "else" block is a very common
pattern. Languages like Python and GDScript accommodate this with an
``elif`` keyword. GDLisp accommodates this pattern with the ``cond``
expression, the general-purpose conditional dispatch form. ``cond``
takes the following form.

::

   (cond (conditional1 branch1) (conditional2 branch2) ...)

That is, the word ``cond`` is followed by several lists. Each list
consists of a conditional expression, similar to the conditional
portion of ``if``, followed by one or more expressions. When
evaluated, the ``cond`` form evaluates each conditional. When it
encounters one that's true, it stops, evaluates that branch, and
returns the result. Our ``if`` above can be translated as follows::

  (cond
    ((> x 0) (print "x is positive!"))
    ((< x 0) (print "x is negative!"))
    (#t (print "x is zero!")))

If none of the branches match, then ``cond`` silently returns ``()``,
though in many cases (including our example above), it's common to
include a final "catch-all" clause whose conditional is the literal
true ``#t`` value.

Like ``if``, ``cond`` is an expression and returns values, so the
entire ``cond`` expression can be assigned to a variable or passed as
a function argument.

.. Note:: Incidentally, ``cond`` is the only primitive conditional
          expression in GDLisp. ``if``, ``when``, ``unless``, ``and``,
          and ``or`` are all macros built on top of ``cond``, while
          ``cond`` is baked into the compiler.

Comparisons
^^^^^^^^^^^

We've already seen the ``<`` and ``>`` functions, which compare values
for, respectively, the less-than and greater-than relations. The
``<=`` and ``>=`` functions also exist for non-strict comparisons.
Finally, ``=`` is used for equality and ``/=`` is used for inequality.
Comparison, ordering, and equality semantics are equivalent to the
corresponding Godot operators.

All of these functions accept variable arguments and are applied
transitively to their arguments. Concretely, that means that ``(< a b
c d)`` is true if and only if ``a`` is less than ``b``, ``b`` is less
than ``c``, and ``c`` is less than ``d``. The other comparison
operators work similarly.

Of particular note is ``/=``, which is unique among the comparison
operators in that it is not transitive. ``(/= a b c d)`` is true if
and only if *none* of the arguments are equal. It compares every
possible pairing of arguments, not just the adjacent ones. More
concretely, ``(/= 1 2 1 3)`` is false, since one is equal to itself.

Since the ``=`` function obeys Godot's built-in equality semantics, it
compares dictionaries by reference. The GDLisp function ``equal?``
works like ``=`` but for deep equality. ``equal?`` is similar to the
GDScript function ``deep_equal``, except that the former accepts
variable arguments and applies the equality relationship transitively.

Looping Expressions
-------------------

Like GDScript, GDLisp provides constructs for repeating code.

While Loops
^^^^^^^^^^^

The simplest looping construct is a ``while`` loop. To print all of
the numbers from 10 down to 0, we can write::

   (let ((x 10))
     (while (> x 0)
       (print x)
       (set x (- x 1))))

The ``while`` form takes a conditional expression as its first
arguments, and the loop body, which can consist of zero or more
expressions, follows. When a ``while`` loop is evaluated, the
conditional is evaluated. If the conditional is true, then the body is
evaluated and the process repeats. If the conditional is false, then
the body is skipped. ``while`` loops always return ``()``.

GDLisp has no direct equivalent of a "do-while" loop from other
languages. However, the construct is easy enough to mimic. Since the
conditional part of a ``while`` loop can be *any* expression (even a
``progn`` form), we can simply place the entire loop body inside the
conditional part and leave the body empty.

::

   (let ((x 10))
     (while (progn (print x)
                   (set x (- x 1))
                   (> x 0))))

For Loops
^^^^^^^^^

For loops in GDLisp work exactly as they do in GDScript.

::

   (for i (range 10)
     (print x))

``for`` takes a variable name, an iterable object (such as an array or
a string), and a body. It evaluates the body once for each element of
the iterable, with the variable bound to that element at each
iteration. Like ``while``, a ``for`` loop always returns ``()``.

Breaking and Continuing
^^^^^^^^^^^^^^^^^^^^^^^

The built-in special forms ``(break)`` and ``(continue)`` work like
the equivalent GDScript keywords. ``(break)`` can be used inside of a
loop and, when evaluated, will immediately exit the loop.
``(continue)`` can be used inside a loop and jumps back to the
beginning of the loop, beginning the *next* iteration of the loop (or
exiting the loop, if we were on the final iteration).

How NOT to Loop
^^^^^^^^^^^^^^^

Now that we've seen the two basic looping constructs in GDLisp, it's
important to emphasize that there are often alternatives. In an
imperative language like GDScript or Java, most iteration is done, as
a matter of course, with ``for`` or ``while``. However, GDLisp is a
functional programming language, and as such it provides several
higher-order functions designed to capture common looping and
iteration patterns. This section aims to provide an incomplete summary
of some of the useful functions that can be used to replace common
looping patterns.

Transforming each Element of an Array
"""""""""""""""""""""""""""""""""""""

Consider this GDScript code.

.. code-block:: gdscript

   var new_array = []
   for element in old_array:
       new_array.push_back(element * 2)
   return new_array

This block of code takes an array ``old_array``, doubles each element,
and returns a new array containing the doubles. This pattern of
applying an operation to each element of an array is captured by the
:ref:`function-array-map` function. The idiomatic way to write the
above code in GDLisp is

::

   (array/map (lambda (x) (* x 2)) old-array)

Summing or Combining all Elements of an Array
"""""""""""""""""""""""""""""""""""""""""""""

This GDScript code sums the elements of an array.

.. code-block:: gdscript

   var total = 0
   for element in old_array:
       total += element
   return total

The pattern of accumulating all of the elements of an array using some
binary function is called a *fold*, and it can be done in GDLisp with
:ref:`function-array-fold`.

::

   (array/fold #'+ old-array 0)

Discarding some Elements of an Array
""""""""""""""""""""""""""""""""""""

The following GDScript code takes an array and keeps only the elements
which are positive.

.. code-block:: gdscript

   var new_array = []
   for element in old_array:
       if element > 0:
           new_array.push_back(element)
   return new_array

This pattern is captured by the :ref:`function-array-filter` function.

::

   (array/filter (lambda (x) (> x 0)) old-array)

Searching an Array for some Matching Element
""""""""""""""""""""""""""""""""""""""""""""

This GDScript code searches an array, in order, for an element
satisfying a particular condition, returning the first match.

.. code-block:: gdscript

   for element in old_array:
       if element % 10 == 0:
           return element
   return null

This can be done in GDLisp with :ref:`function-array-find`.

::

   (array/find (lambda (x) (= (mod x 10) 0)) old-array)

``array/find`` also optionally accepts a third argument, which
defaults to ``()`` and is returned if no match is found.

Locals
------

Local Variables
^^^^^^^^^^^^^^^

As we start to write larger blocks of code, it will become convenient
to store the results of intermediate expressions in local variables.
We've already seen examples that use the most basic primitive:
``let``.

::

   (let ((variable-name1 initial-value1)
         (variable-name2 initial-value2) ...)
     ...)

The ``let`` form takes a list of variable clauses and then a body. The
given variables are declared and bound to their initial values. Then
the body is evaluated in a scope where the local variables exist.

You can declare any number of variables in a ``let`` clause.
Crucially, the variables' scope is bound strictly to the ``let``
block, so as soon as the body of the ``let`` finishes evaluating, the
variables are no longer accessible. This is in contrast to GDScript,
where a ``var`` declaration implicitly lasts until the end of the
*enclosing* scope. In GDLisp, a local variable always defines its own
scope when it's declared.

Example::

  (let ((a 1) (b 2))
    (+ a b)) ; 3

Note that when a ``let`` block has multiple variable clauses, the
clauses are evaluated in parallel.

::

   (let ((x 1) (y (+ x 1))) y)

In this example, the ``x`` from the first clause is *not* in scope
when the ``y`` clause is evaluated. If there's an ``x`` from an
*enclosing* scope available, then ``y`` will be set to that variable
plus one. If not, this code will produce an error at compile-time.

It's common to want several variable bindings to be evaluated in
order, such as when initializing several pieces of data to perform
some calculations. For this use case, GDLisp provides the
:ref:`macro-let-star` macro.

::

   (let* ((x 1) (y (+ x 1))) y) ; 2

``let*`` works exactly like ``let`` and shares all of the same syntax,
except that the variables in a ``let*`` are bound sequentially, with
each subsequent variable having access to all of the prior ones. In
effect, a ``let*`` form is equivalent to several nested ``let`` forms,
each having only one variable clause.

To change the value of an existing local variable, use ``set``.

::

   (let ((x 1))
     (print x) ; 1
     (set x (+ x 1))
     (print x)) ; 2

Note that local variables, even mutable local variables, fully support
closures. That is, if a ``lambda`` or other local function construct
closes around a local variable, then both the closure function and the
enclosing scope can modify the variable, and both scopes will reflect
the change.

Local Functions
^^^^^^^^^^^^^^^

Similar to ``let``, GDLisp provides the ``flet`` primitive for
declaring one or more local functions. This is most useful when you
have a private function that is only needed inside one function.

::

   (flet ((normalize-name (name) ((name:capitalize):substr 0 10)))
     (print "Hello, " (normalize-name first-name) (normalize-name last-name))
     (print "You have an unread message from " (normalize-name caller-name)))

In this hypothetical example, we need to print some messages involving
people's names. For consistency in our hypothetical UI, we want all
names to be capitalized in a particular way and to truncate their
length to ten characters. We can define the function to normalize
these names as a local function with ``flet`` and then call it as many
times as we want inside the ``flet`` block. Outside the block, the
function doesn't exist.

``labels`` is a variant of ``flet`` with the same syntax but more
powerful binding semantics. ``labels`` evaluates its function bodies
in a scope where all of the function clauses already exist. This
allows your local helper functions to be recursive or to depend on
each other in any order.

.. Tip:: If you're declaring a local function with the intent of
         passing it (as an argument) to a higher-order function like
         ``array/map`` or ``array/filter``, you'll have better luck
         using an ordinary ``let`` and a ``lambda``, since you need to
         reify the function as a value anyway. ``flet`` and ``labels``
         are intended for situations where you want to locally *call*
         the function inside the scope.
