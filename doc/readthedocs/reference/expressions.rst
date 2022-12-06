
Expressions
===========

Expressions are the forms that make up the body of functions and the
initial value of variables and constants. Expressions are the basic
unit of "work" in a GDLisp program.

Note that GDLisp is an expression-oriented language, which means that
there is no distinction between statements and expressions in GDLisp.
Even though we may colloquially refer to the ``if`` special form as an
"if statement", it is an expression like any other. This commitment to
expressions allows for better composability, as any expression can be
nested inside of any other.

There are several different types of expressions, all of which are
detailed below.

Atoms
-----

There are several S-expressions which compile natively to themselves.
These include the null object ``()``, integer literals, floating-point
literals, string literals, and the Boolean constants ``#t`` and
``#f``. Note that this explicitly does *not* include symbol literals,
which are evaluated differently. See :ref:`expr-variable-names` below.

.. _expr-variable-names:

Variable Names
--------------

A bare symbol appearing in expression context is treated as a variable
name. All variables in GDLisp are lexically-scoped. That is, any
variable is declared to be either scoped to the current module or to a
narrower scope, such as a ``let`` block or a function body. It is an
error to refer to a variable name that does not exist.

A bare symbol in expression context always refers to a name in the
value namespace. Variable names can be *shadowed*, which means that a
bare symbol always refers to the name in the *narrowest scope* for
which it is defined.

Note that bare symbols appearing in the code are subject to macro
expansion for :ref:`symbol macros <symbol-macros>`.

Function Calls
--------------

A proper, nonempty list will be interpreted in one of several ways,
depending on the value of the first term (or head) of the list. The
head of a function call can be one of three things. It can be a symbol
literal, a ``literally`` form, or an ``access-slot`` form.

Ordinary Calls
^^^^^^^^^^^^^^

An ordinary call is one whose head is a symbol literal. Ordinary calls
can resolve to one of several forms, depending on the value of the
head.

First, if the value of the head is the start of one of GDLisp's
:ref:`special forms <expr-special-forms>`, then the call is treated as
a special form, even if there is a function name in the current scope
which would shadow that special form. These special forms are baked
into the GDLisp compiler and are the primary bootstrapping tool on
which we build up syntax. There is no way to define new special forms
in GDLisp.

If the head is not the start of one of the special forms, then it must
be a valid name in the function namespace of the current lexical
scope. Like values, names in the function namespace are always
accessed starting from the innermost scope and falling back to outer
scopes, which makes names in the function namespace subject to
shadowing as well.

If the name refers to a macro, then :ref:`macro expansion <macros>`
occurs. If the name refers to an ordinary function, then the call
compiles to a runtime call to the function with the given name.

Literal Calls
^^^^^^^^^^^^^

A literal call is a call whose head is the form ``(literally name)``,
where ``name`` is an arbitrary symbol. ``literally`` calls are like
:ref:`literal forms <expr-literal-forms>`, except that they are
function calls rather than values. That is, a literal call always
expands to a call to the function with the given name in the current
scope in GDScript, without regard to whether or not that call is valid
or makes sense. ``literally`` calls never undergo macro expansion and
are never considered special forms.

All of the same caveats that apply to literal forms apply to literal
calls, and they should be used with caution.

Method Calls
^^^^^^^^^^^^

A method call is a call whose head is of the form ``(access-slot expr
name)``, where ``expr`` is an arbitrary expression and ``name`` is a
literal symbol. In cases where the operator precedence is not
ambiguous, these ``access-slot`` forms are usually written using the
``:`` notation as ``expr:name``. That is, to call the method named
``bar`` on an object called ``foo``, with three arguments, we would
write ``(foo:bar 1 2 3)``, which is syntax sugar for ``((access-slot
foo bar) 1 2 3)``.

The first argument to ``access-slot`` is an arbitrary expression, on
which the method will be called, and ``name`` will be :ref:`normalized
<name-normalization>` to the name of the method to call on the object.

GDLisp does *not* validate the name of the method or that it exists on
the object referred to by the expression.

If the first argument to ``access-slot`` is the literal name
``super``, then the method call is a :ref:`super call
<expr-super-calls>`.

.. _expr-super-calls:

Super Calls
"""""""""""

::

   (super:method-name args ...)

A method call where the left-hand side of the ``access-slot`` call is
the literal symbol ``super`` is a super call. A super call can only
occur in a context where the ``self`` name exists (i.e. inside of a
class body) and will call the method with the given name, using
``self`` as the target of the call, but only considering methods
defined in the superclass of the current class body.

Note that calls to a superclass' *constructor* are handled specially.
Specifically, they are *not* written as ``super:_init``, and in fact
they are not even expressions in the strictest sense of the word. A
call to a superclass' constructor is a special part of the ``_init``
definition syntax. For more details, see :ref:`constructor-functions`.

.. _expr-special-forms:

Special Forms
-------------

Special forms are elements of the GDLisp syntax that have special
meanings baked into the compiler. Special forms can be thought of as
similar to macros but more primitive, the building blocks on which
GDLisp syntax is constructed. There are 25 special forms in GDLisp.

``progn`` Forms
^^^^^^^^^^^^^^^

::

   (progn args ...)

A ``progn`` form evaluates each of its arguments in order and returns
the final argument. ``progn`` is a useful way to insert multiple
expressions which have side effects in a context, such as the
right-hand side of a ``defvar``, that only accepts one expression.

An empty ``progn`` silently returns ``()``, the null object.

Note that ``progn`` can also be used in declaration (or class
declaration) context. See :ref:`progn` for details.

``cond`` Forms
^^^^^^^^^^^^^^

::

    (cond clauses ...)

A ``cond`` form is the most basic form of conditional in GDLisp.
``cond`` takes zero or more clauses. Each clause's conditional portion
is evaluated in turn. If the conditional is true, then the clause's
body portion is evaluated and returned. Otherwise, the next clause
is tried. If all clauses are exhausted, then the null object ``()`` is
returned.

Each clause must be a proper list containing one or more elements. If
the list contains at least two elements, then the first element is the
conditional term and the rest form the body of the clause. The body is
treated as though it is inside a ``progn``, so the last expression
will be returned. If the list contains only one element, then that
element is *both* the condition and the body of the clause, and it
will only be evaluated once.

For example,

::

   (cond
     ((foo1) (bar1) (baz1))
     ((foo2) (bar2)))

This is a ``cond`` form consisting of two clauses. When this form is
evaluated, first, we will call the function ``foo1`` with no
arguments. If that function returns a truthy value, then we call
``bar1`` and then ``baz1``, using the latter as the result of the
whole ``cond`` form. If ``foo1`` returns a falsy value, then we try
``foo2``. If ``foo2`` evaluates to a truthy value, then ``bar2`` is
evaluated and its result is returned. Otherwise, ``()`` is returned as
a default value.

As an example of the one-argument clause form, consider

::

   (cond
     ((my-dict:get "x"))
     ((my-dict:get "y"))
     ((my-dict:get "z")))

Assuming ``my-dict`` is a dictionary object, this expression will
attempt to get the keys ``x``, ``y``, and then ``z`` from the
dictionary in order, returning the first one which exists and is
truthy. If none satisfy the condition, then ``()`` is returned as a
default value.

A common idiom is to make the condition of the last clause be the
literal ``#t`` true object. This acts as a sort of "else" clause,
triggering unconditionally if all of the other branches fail.

``while`` Forms
^^^^^^^^^^^^^^^

::

   (while condition body ...)
   (while condition)

A ``while`` form is one of the two most basic forms of looping in
GDLisp. A ``while`` form takes a condition and then zero or more
expressions forming a body as arguments. The ``while`` loop iterates
zero or more times. At each iteration, the loop runs the condition
first. If the condition is falsy, then the loop exits immediately.
Otherwise, the body runs, and then the loop starts over.

A ``while`` loop always returns the null object ``()``. It is possible
to have a ``while`` loop where the body is empty, in which case, the
condition is evaluated multiple times until it returns a falsy value.
This can be used to emulate the "do ... while" construct seen in some
programming languages, where the condition is evaluated at the end of
the body, rather than the beginning. That is, to emulate such a
construct in GDLisp, consider

::

   (while (progn
     body ...
     condition))

``for`` Forms
^^^^^^^^^^^^^

::

   (for var iterable body ...)

A ``for`` form is the second of the two most basic looping constructs
in GDLisp. The first argument to ``for`` must be a literal symbol,
then the other arguments are arbitrary expressions. First,
``iterable`` is evaluated once, and it must evaluate to an array
(including pool arrays), string, or dictionary object. Then a new
lexical scope is created. Then ``body`` is run in that lexical scope,
once for each element of the iterable object. At each loop iteration,
the variable ``var`` is bound to the current value. ``for`` forms
always return the null object ``()``.

For arrays, a ``for`` form iterates over each element of the array.
For dictionaries, a ``for`` form iterates over each *key* of the
dictionary, consistent with Python's semantics for the same. For
strings, a ``for`` form iterates over each character (as a
single-character string) of the string.

Note that the behavior is undefined if the result of ``iterable`` is
not an array, dictionary, or string. Currently, ``for`` loops in
GDLisp compile to ``for`` loops in GDScript, which means some legacy
GDScript behavior (such as iterating over numerical literals) may
work, but this behavior may change in the future, so it is always
recommended to explicitly call ``range`` if the intent is to iterate
up to a number.

.. _expr-literal-forms:

``literally`` Forms
^^^^^^^^^^^^^^^^^^^

::

   (literally variable-name)

A ``literally`` form is a backdoor through the GDLisp scoping system.
The sole argument to ``literally`` must be a symbol literal.
``(literally x)`` will be translated into the variable name ``x`` in
the resulting GDScript code. This will be done **without any
consideration** to whether or not ``x`` is a valid variable name.
GDLisp will not check that the name is defined, or what scope it is
defined in. GDLisp will merely assume that you know what you're doing
and pass the name through.

The name ``variable-name`` given to this form will undergo a partial
form of :ref:`name normalization <name-normalization>`. Specifically,
``variable-name`` will be escaped in the same way as an ordinary
variable name, with the exception that GDScript reserved words will
not be prefixed with an underscore.

Care must be taken when using ``literally``. Since GDLisp does not
perform any semantic analysis on the given name, it cannot guarantee
that the name is valid, or even syntactically makes sense in GDScript
in the case of keywords. Additionally, names referenced inside of
``literally`` will not have closures created for them if they occur
inside of a ``lambda`` or other closure-producing construct. This can
result in difficult-to-debug situations that GDLisp cannot handle.

The primary intended use case for ``literally`` is to port future
GDScript functions to GDLisp without having to wait on official
support from the GDLisp compiler. If a future iteration of Godot adds
a function called ``frobnicate`` to the global namespace, then you can
call that function by using the name ``(literally frobnicate)``, even
if the version of the GDLisp compiler you're using is not aware that
such a function exists.
