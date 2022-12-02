
Lambda Lists
============

Functions, macros, and constructors in GDLisp take arguments. As part
of their declaration syntax, any of these declarations must declare
the collection of *formal arguments* that they take. These formal
arguments take the form of a *lambda list*, which is a special
sub-language that communicates what arguments are permitted to the
GDLisp compiler.

Broadly speaking, a lambda list is a list of symbols indicating
variable names to bind arguments to. As part of this list of symbols,
special directives beginning with an ampersand ``&`` can change the
behavior of the following arguments, such as making required arguments
optional.

Note that a name beginning with an ampersand in a lambda list is
*always* treated as a special directive. It is an error to use a
special directive that does not exist, or to use a directive in a
context that disallows it. As a consequence, it is not possible to
declare arguments in a lambda list whose names begin with an
ampersand. It is generally not recommended to declare variable names
beginning with an ampersand at all for this reason.

It is an error to list the same name twice in a lambda list. Likewise,
it is an error to list the same special directive twice, unless
otherwise stated.

A special directive generally only lasts until the next special
directive. Unless otherwise stated, directives do not "stack".

There are different types of lambda lists, depending on the context in
which it appears.

.. _simple-lambda-lists:

Simple Lambda Lists
-------------------

The simplest kind of lambda list is called, fittingly, a *simple
lambda list*. A simple lambda list is a collection of zero or more
required arguments, with no special directives or alternative forms
allowed. That is, a simple lambda list is always a simple list of
literal symbols.

Simple lambda lists are used in the declaration of class functions and
signals, which do not support more advanced argument parsing.

Examples::

  ()
  (arg1 arg2)
  (foo bar baz)

.. _ordinary-lambda-lists:

Ordinary Lambda Lists
---------------------

Ordinary lambda lists are the most common kind of lambda list in
GDLisp, used for module-level functions and macros, as well as lambda
expressions.

An ordinary lambda list is a list of symbol names of arguments. An
ordinary lambda list accepts several special directives. All special
directives are optional. If multiple directives are provided, then
``&opt`` must be the first of them, and only one of ``&rest`` or
``&arr`` can be provided.

* Any arguments that appear before any directives are treated as
  *required parameters*. It is an error to call a function or macro
  with fewer arguments than the number of required parameters in its
  lambda list.

* Any arguments that appear after the ``&opt`` directive are optional
  parameters. If too few arguments are provided and the rest of the
  parameters are optional, then any optional parameters that have not
  been bound receive a default value of ``()``, the null object.

* If the ``&rest`` directive is used, it must be followed by a single
  name, which indicates the name of the function's "rest" argument.
  After binding any required and optional arguments, all remaining
  arguments to the function are collected into a list and passed as
  the function's "rest" argument. If there are no extra arguments,
  then the empty list ``()`` is passed. Note that it is impossible to
  call a function which declares a ``&rest`` argument with "too many"
  arguments, as all extras will be collected into one variable.

* The ``&arr`` directive works similarly to ``&rest``. It must be
  followed by a single name. All remaining arguments, after binding
  required and optional arguments, will be collected and passed at the
  "arr" argument, but in this case they will be collected into an
  array rather than a list.

Examples::

  ()
  (arg1 arg2)
  (required-arg1 required-arg2 &opt optional-arg1 optional-arg2)
  (&rest all-args)
  (&arr all-args-as-array)
  (required-arg &opt optional-arg &rest all-remaining-args)

.. _constructor-lambda-lists:

Constructor Lambda Lists
------------------------

A constructor lambda list is a lambda list used to indicate the
arguments to a class' ``_init`` constructor. A constructor lambda list
is similar to a simple lambda list but allows for one additional
feature.

A constructor lambda list consists of a list of arguments. Each
argument can be a symbol, similar to a simple lambda list. However,
arguments can also take the special form ``@name`` (or, written
literally, ``(access-slot self name)``), where ``name`` is any valid
symbol identifier. An argument of this form will *not* be bound to a
local variable when the constructor is called. Instead, the value
passed for that argument will be bound to the instance variable with
the given name on ``self``. If no such instance variable exists, then
an error will be issued at runtime.

This allows a very compact representation of classes whose
constructors merely initialize fields.

.. code-block::

   (defclass Enemy (Node)
     (defvars name attack-points defense-points)
     (defn _init (@name @attack-points @defense-points)))

Examples::

  (foo bar)
  (x y z)
  (@x @y @z)
  (some-ordinary-arg @some-instance-var)
