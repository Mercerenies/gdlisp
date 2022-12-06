
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

A ``while`` loop defines a loop context, for both its condition and
its body. This means that ``break`` and ``continue`` can be used in
either the condition or the body.

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

A ``for`` loop defines a loop context for its body. This means that
``break`` and ``continue`` can be used in the body of a ``for`` loop.

``let`` Forms
^^^^^^^^^^^^^

::

   (let (clauses ...) body ...)

``let`` is the most basic form of local variable binding in GDLisp. A
``let`` form creates a new lexical scope in which zero or more local
variables are bound, and then runs ``body`` in that local scope. The
value of the final expression of ``body`` is returned, or ``()`` if
``body`` is empty.

Each variable clause takes one of the following forms.

::

   var-name
   (var-name initial-value ...)

In the second (and most general) form, a variable clause takes the
form of a proper list whose first element is a literal symbol
indicating the name of the variable to declare. The remaining elements
are evaluated to determine the variable's initial value. Note
carefully: the ``initial-value`` expressions are evaluated in the
*outer* scope, not in the newly-created scope that the variable is
being declared in. This means that, in a ``let`` statement which
declares multiple variables, none of the variables have access to each
other during initialization, even those declared later in the same
block. The ``initial-value`` block is treated as a ``progn`` block, so
if the block is empty then ``()`` is used as the variable's initial
value.

A variable name ``var-name`` that appears on its own (that is, a
symbol literal *not* contained in a sublist) is treated as
``(var-name)`` and will initialize the variable to ``()``.

``let`` always binds in the value namespace. To bind in the function
in the function namespace, see :ref:`expr-flet` and
:ref:`expr-labels`.

.. _expr-flet:

``flet`` Forms
^^^^^^^^^^^^^^

::

   (flet (clauses ...) body ...)

An ``flet`` form is similar to a ``let`` form except that it binds
functions in the function namespace, rather than arbitrary values in
the value namespace. Specifically, an ``flet`` form creates a new
local scope and defines zero or more functions in that local scope.
Then the body is executed in that scope and its final value returned.

Each function clause takes the following form.

::

   (name (args ...) body ...)

``name`` is a symbol literal indicating the name of the local
function, ``args`` is an :ref:`ordinary lambda list
<ordinary-lambda-lists>`, and ``body`` is the body of the function.
When the function called ``name`` is invoked inside of the ``flet``
form's body, the given arguments will be bound and the body will be
executed in a new lexical scope, cloned from the scope in which
``flet`` itself was defined.

Note that the bodies of the local functions are evaluated in a scope
cloned from the one in which the ``flet`` form was defined, not the
inner scope created by ``flet``. That is, the local function bodies
defined by an ``flet`` do not have access to each other's names or to
their own. For a version of ``flet`` that does have such access, see
:ref:`expr-labels`.

The bodies of ``flet`` local functions can create :ref:`closures
<expr-capture>`. ``flet`` creates a loop barrier between the enclosing
scope and the clauses of the ``flet`` form. This means that a
``break`` or ``continue`` expression inside of a clause of the
``flet`` cannot be used to control a loop that began outside of the
clauses. This constraint does not exist for the body of the ``flet``,
only the clauses.

.. _expr-labels:

``labels`` Forms
^^^^^^^^^^^^^^^^

::

   (labels (clauses ...) body ...)

``labels`` works nearly identically to ``flet`` and carries the exact
same syntax. However, whereas an ``flet`` form evaluates its local
function bodies in the enclosing scope of the ``flet`` block, a
``labels`` form evaluates its function bodies in the inner scope of
the ``labels`` block itself. This means that the functions defined in
a ``labels`` block have access to each other and to their own name,
allowing them to be recursive or mutually recursive.

The bodies of ``labels`` local functions can create :ref:`closures
<expr-capture>`. ``labels`` creates a loop barrier between the
enclosing scope and the clauses of the ``labels`` form. This means
that a ``break`` or ``continue`` expression inside of a clause of the
``labels`` cannot be used to control a loop that began outside of the
clauses. This constraint does not exist for the body of the
``labels``, only the clauses.

``lambda`` Forms
^^^^^^^^^^^^^^^^

::

   (lambda (args ...) body ...)

A ``lambda`` form defines a local function without giving it a name.
The argument list ``args`` is an :ref:`ordinary lambda list
<ordinary-lambda-lists>`. A new function object is created, which
exists as a value (hence, can be assigned to variables in the value
namespace or passed as an argument to a function).

When the function created by this form is invoked, a new lexical scope
is created, which is cloned from the lexical scope in which the
``lambda`` was first defined. Then the arguments are bound and the
body is run, just like any other function.

The ``lambda`` body can create :ref:`closures <expr-capture>`.
``lambda`` creates a loop barrier between the enclosing scope and the
body of the ``lambda`` form. This means that a ``break`` or
``continue`` expression inside of the body of the ``lambda`` cannot be
used to control a loop that began outside of the body.

``function`` Forms
^^^^^^^^^^^^^^^^^^

::

   #'name
   (function name)

The ``function`` special form is used to take a function that exists
in the function namespace and convert it into a first-class value. The
``function`` form is often abbreviated using the (equivalent) syntax
``#'name``.

The ``name`` argument must be a symbol, and it must be a valid name in
the function namespace of the current lexical scope. A function object
is created (as a value) which, when called, invoked the function with
the given name, forwarding all arguments.

The name can refer to a function defined at module scope or to a local
function defined in the current scope. In the latter case, the local
function will be kept alive (by reference semantics) until the
function object constructed by this ``function`` form is discarded.
That is, it is permitted to have a reference to a local function which
outlives the scope of that local function's binding.

``function`` *cannot* be used to create references to instance
methods. Explicit ``lambda`` expressions must be used to do so.

The target name of a ``function`` form must be the name of a valid
function. If the name refers to a macro, then the behavior is
undefined.

``set`` Forms
^^^^^^^^^^^^^

::

   (set variable value)

``set`` is the basic form of variable and name assignment in GDLisp.
It can do several different things, depending on the nature of the
``variable`` portion of the form.

Variable Assignment
"""""""""""""""""""

If ``variable`` is a literal symbol, then it is interpreted as a
variable name in the value namespace. The variable pointed to by that
name is assigned a new value, namely the result of evaluating
``value``. The variable must be mutable, or a compile error will be
issued.

The value that was assigned is returned from the ``set`` form.

Field Assignment
""""""""""""""""

If ``variable`` is of the form ``(access-slot object target)``, where
``object`` is an arbitrary expression and ``target`` is a literal
symbol, then the assignment will modify an instance field on the
instance to which ``object`` evaluates. The field's name shall be
``target``, after :ref:`name normalization <name-normalization>`. This
can trigger :ref:`setter functions <getter-and-setter>`.

The value that was assigned is returned from the ``set`` form. It is
unspecified whether this will invoke a getter function on the class,
if one exists.

Delegated Assignment
""""""""""""""""""""

If ``variable`` is a proper list of the form ``(head args ...)`` where
``head`` is a literal symbol that is *not* ``access-slot``, then the
assignment is a delegated assignment. The form

::

   (set (some-function args ...) value)

will compile into the function call

::

   (set-some-function value args ...)

That is, a ``set`` on a function call will compile to a call to the
function whose name is the former function with ``set-`` prepended to
it. The right-hand side of the assignment will be the first argument
passed to the delegated function. The return value of the function is
returned from the ``set`` form, so by convention a function intended
to be used in this way should return the assigned value.

``quote`` Forms
^^^^^^^^^^^^^^^

::

   's-expression
   (quote s-expression)

A ``quote`` form refuses to evaluate its argument and returns the
S-expression representing it verbatim. See :ref:`quoting` for more
details. Note that a ``quote`` form is usually written abbreviated as
``'s-expression``.

``quasiquote`` Forms
^^^^^^^^^^^^^^^^^^^^

::

   `s-expression
   (quasiquote s-expression)

A ``quasiquote`` form refuses to evaluate its argument and returns the
S-expression representing it, similar to ``quote``. However,
``unquote`` and ``unquote-spliced`` have special meaning inside of
``quasiquote`` forms. See :ref:`quoting` for more details.

``unquote`` Forms
^^^^^^^^^^^^^^^^^

::

   ,expr
   (unquote expr)

An ``unquote`` form can only be used inside of a ``quasiquote`` form.
It is an error for this special form to appear in an expression
context.

``unquote-spliced`` Forms
^^^^^^^^^^^^^^^^^^^^^^^^^

::

   ,.expr
   (unquote-spliced expr)

An ``unquote-spliced`` form can only be used inside of a
``quasiquote`` form. It is an error for this special form to appear in
an expression context.

``access-slot`` Forms
^^^^^^^^^^^^^^^^^^^^^

::

   expr:field
   (access-slot expr field)

The ``access-slot`` form, usually written using the infix ``:``
notation, accesses a field on an object. That is, ``expr`` is
evaluated, and then the slot with the name ``field`` (which must be a
symbol literal) is returned from the object referenced by the
expression.

The field name is not validated. That is, GDLisp makes no effort to
ensure that the name ``field`` is a field that exists on the type of
``expr``. There is one exception to this rule. If ``expr`` is the
literal name of an enumeration (a la ``defenum``) whose definition is
statically known, GDLisp will validate that the name ``field`` is
actually a defined enumeration constant on that type.

``new`` Forms
^^^^^^^^^^^^^

::

   (new Superclass body ...)
   (new (Superclass args ...) body ...)

The ``new`` form constructs a new local *anonymous* class. That is,
``new`` is to ``defclass`` as ``lambda`` is to ``defn``. The
newly-defined class is not given a name, and the only instances of
that class are those created by this particular ``new`` form.

When this form is evaluated, an instance of a subclass of
``Superclass`` is constructed. The body of this subclass shall be
``body``, which can consist of zero or more :ref:`class declarations
<classes>`, with the exception that it is illegal to define static
methods in an anonymous class. The constructor of this class shall be
invoked with ``args``, or with zero arguments if the non-parameterized
version of ``new`` is used.

The body of a ``new`` statement is capable of creating :ref:`closures
<expr-capture>`.

``new`` creates a loop barrier between the enclosing scope and the
body of the ``new`` form. This means that a ``break`` or ``continue``
expression inside of the body of the ``new`` cannot be used to control
a loop that began outside of the body.

**Note:** ``new`` is *not* a general-purpose constructor. Programmers
 used to Java or C# may be used to prefixing type names with ``new``
 to construct ordinary instances of the type. That is not how object
 construction works in GDLisp. To construct ordinary instances of some
 class, call the method ``new`` on that class, such as
 ``(ClassName:new 1 2 3)``. The ``new`` special form is only intended
 to be used when behavior (such as instance variables or methods) is
 being added anonymously to the class for this instance alone.

``yield`` Forms
^^^^^^^^^^^^^^^

::

   (yield)
   (yield object signal)

The ``yield`` special form behaves similarly to the GDScript function
of the same name. Called with zero arguments, ``yield`` halts the
current function and returns a function state object from the
function. That function state object has a ``:resume`` method which
will return to the halted function at the same point it was yielded
from.

If ``yield`` is called with two arguments, then both arguments are
evaluated. The first is treated as an object and the second shall
evaluate to a string which is the name of a signal on the given
object. The function still halts and returns a state object, just as
if ``yield`` was called with no arguments. However, if the given
object ``object`` ever fires the signal called ``signal``, then the
function resumes automatically, without an explicit call to
``:resume``.

It is an error to call ``yield`` with exactly one argument.

Note that ``yield`` is a special form, not a function, despite it
evaluating its arguments in applicative order. Functions in GDLisp
must satisfy `η-reduction
<https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-reduction>`_.
That is, in order for ``yield`` to be a function, it would have to be
the case that ``yield`` and ``(lambda (&rest args) (apply #'yield
args))`` are equivalent functions. This is not true for ``yield``,
since the former, when called, will halt the current function, whereas
the latter will halt an inner function and return a (somewhat useless)
function state object that resumes at the end of the inner function.

``assert`` Forms
^^^^^^^^^^^^^^^^

::

   (assert condition)
   (assert condition message)

The ``assert`` special form evaluates the condition and message as
expressions. If ``condition`` is true, then the code proceeds as
planned. If ``condition`` is false, then an error is generated, using
``message`` in the error message if provided.

``assert`` special forms are only used when the resulting Godot
runtime is in debug mode. In release mode, these forms will be
ignored, and their arguments will not even be evaluated. As such,
arguments which have side effects should generally not be given to
``assert``.

``return`` Forms
^^^^^^^^^^^^^^^^

::

   (return expr)

Evaluates the expression and then returns that expression immediately
from the enclosing function or instance method.

``macrolet`` Forms
^^^^^^^^^^^^^^^^^^

::

   (macrolet (clauses ...) body ...)

A ``macrolet`` form is syntactically identical to an ``flet``.
However, whereas ``flet`` binds functions in the function namespace of
the current scope, ``macrolet`` binds *macros* in the same namespace.
The macros defined by a ``macrolet`` are only defined inside of the
``body`` scope. During that scope, those names are subject to macro
expansion.

The clauses of a ``macrolet`` form may **not** create closures. If a
locally-defined macro depends on a local variable or function defined
in an enclosing scope, then the behavior is undefined.

``macrolet`` creates a loop barrier between the enclosing scope and
the clauses of the ``macrolet`` form. This means that a ``break`` or
``continue`` expression inside of a clause of the ``macrolet`` cannot
be used to control a loop that began outside of the clause. This
constraint does not exist for the body of the ``macrolet``.

``symbol-macrolet`` Forms
^^^^^^^^^^^^^^^^^^^^^^^^^

::

   (symbol-macrolet (clauses ...) body ...)

A ``symbol-macrolet`` clause binds local macros, just like
``macrolet``, but the former binds *symbol* macros, which are subject
to macro expansion when a literal symbol is used.

Each clause is of the form

::

   (name value)

Both parts are mandatory. ``name`` is the name of the symbol macro
(which will be bound in the value namespace). ``value`` is the
expression which should be run to evaluate the macro.

Like ``macrolet``, the clauses of a ``symbol-macrolet`` may **not**
create closures. It is undefined behavior to write a local symbol
macro that depends on a local variable or function defined in an
enclosing scope.

``symbol-macrolet`` creates a loop barrier between the enclosing scope
and the clauses of the ``symbol-macrolet`` form. This means that a
``break`` or ``continue`` expression inside of a clause of the
``symbol-macrolet`` cannot be used to control a loop that began
outside of the clause. This constraint does not exist for the body of
the ``symbol-macrolet``.

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

``preload`` Forms
^^^^^^^^^^^^^^^^^

::

   (preload name)

``preload`` is a special form which takes a single string literal as
argument. The string literal must be the name of a file which can be
imported, using the Godot ``res://`` notation. ``preload`` works like
the built-in function ``load`` but performs the act of loading at
compile-time. It is an error if the pathname does not point to a file.

Note that, in GDLisp, most ``preload`` calls should be replaced with
``use`` directives. See :ref:`imports` for details. ``preload`` can be
used in situations (such as macro expansion) where the name being
loaded may not be known at definition time but will be known before
compilation is complete.

``break`` Forms
^^^^^^^^^^^^^^^

::

   (break)

``break`` is a special form that can only be used inside of loop
contexts. ``break`` exits the current loop and continues immediately
after the loop body.

``continue`` Forms
^^^^^^^^^^^^^^^^^^

::

   (continue)

``continue`` is a special form that can only be used inside of loop
contexts. ``continue`` exits the current iteration of the loop and
continues at the top of the loop body.

.. _expr-capture:

Closures and Name Capture
-------------------------

Several special forms create *closures*. A closure is a nested scope
that can outlive its containing scope. We call the variables which are
placed inside a closure for such forms *captures*. This is perfectly
acceptable. If a variable is defined in a local scope and then
captured by a ``lambda`` or other special form, then that variable
will remain in existence for as long as the ``lambda`` object exists.

Function objects always have ``Reference`` semantics, which means that
a function object (created with ``lambda`` or ``function``) will be
freed when the last reference to it is freed. This ensures that
closures created in this way are freed promptly. Custom objects
created with ``new`` will follow the semantics of their superclass
eventual (``Reference`` subclasses will have reference semantics,
while ``Object`` and ``Node`` subclasses will have to be freed
explicitly), so some care must be taken in those situations to prevent
a memory leak.

A closure is a read-write binding to a variable name. That means that
the values in a closure are captured by *reference*, not by value. If
the inside of a closure (such as a ``lambda``) modifies a captured
variable, then the enclosing scope (and, by extension, any other
closure that captured the same variable) will be able to see that
change.

For example, this lambda will return one number higher each time it's
called.

::

   (let ((accum 0)) (lambda () (set accum (+ 1 accum))))

We can call this function, for example, as follows.

::

   (defn create-counter ()
     (let ((accum 0))
       (lambda () (set accum (+ 1 accum)))))

   (defn _ready ()
     (let ((counter (create-counter)))
       (print (funcall counter))   ; Prints 1
       (print (funcall counter))   ; Prints 2
       (print (funcall counter)))) ; Prints 3

The variable ``accum`` is captured by the ``lambda`` *by reference*,
so when we modify the variable, that modification is reflected in
future calls to the ``lambda``, since there is truly only one copy of
that variable.

Forms that define local macros can never capture local variables or
functions from an enclosing scope.
