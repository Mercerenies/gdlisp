
.. _quoting:

Quoting and Quasiquoting
========================

Quoting an S-expression delays its evaluation. That is, normally in
GDLisp, a symbol or proper list that appears in expression context
will have special semantics, whether that involves expanding macros,
applying functions, accessing local variables, or any number of other
rules. However, if you actually intend to explicitly construct a list
or symbol as-is and have access to that data structure as a runtime
value, you must quote the S-expression.

This is done by calling the special form ``quote`` with one argument,
as ``(quote some-s-expression)``. This can be abbreviated to an
apostrophe followed by the expression: ``'some-s-expression``.

All literals evaluate to themselves when quoted. This includes
Booleans (``#t`` and ``#f``), integers, floats, strings, symbols, and
the null object. Aside from symbols (when are treated specially when
quoted), the quote operator is a no-op on literals. That is, the
following are all equivalent when evaluated.

::

   '1 == 1
   '1.0 == 1.0
   '#t == #t
   '#f == #f
   '() == ()
   '"abc" == "abc"

A list evaluates, likewise, to the list itself when quoted. So while
``(foo bar)`` is a function call in expression context, ``'(foo bar)``
(or equivalently ``(quote (foo bar))``) is a proper list at runtime,
whose first element is the symbol ``foo`` and whose second is the
symbol ``bar``. Note that this also highlights another property of
quoting, namely that quoting an expression is contagious on the inner
expressions. Since the outer list in ``'(foo bar)`` is quoted, the
names ``foo`` and ``bar`` are never evaluated as variable names
either.

You may also use dotted list notation to construct lists with
non-``()`` endings. For instance, ``'(1 2 . 3)`` will return a runtime
cons object whose car is ``1``. The cdr of this object is another cons
object whose car is ``2`` and whose cdr is ``3``.

Quasiquoting
------------

Often, fully quoting with ``quote`` is sufficient. However, there are
often situations, especially during macro expansion, where an
S-expression should be interpreted mostly literally but with a few
values interjected at runtime. This is where quasiquoting comes in.

An S-expression is quasiquoted with the ``quasiquote`` special form,
abbreviated with a single prefix backtick `````. ``quasiquote``
behaves like ``quote`` except that it interprets the forms ``unquote``
and ``unquote-spliced`` in a special way.

An ``unquote`` form (or, equivalently, a prefix ``,``) inside of a
``quasiquote`` effectively reverses the ``quasiquote``. The expression
inside of the ``unquote`` is evaluated and *interpolated* into the
list or expression being constructed by ``quasiquote``.

An ``unquote-spliced`` form (equivalently, a prefix ``,.``) works like
an ``unquote``, except that it can only be used inside a *list* within
a ``quasiquote`` and will flatten itself inside the enclosing list.
Consider the following examples.

::

   `(1 2 ,(list 3 4) 5)

This evaluates to ``(1 2 (3 4) 5)``. The ``unquote`` (written with a
comma) causes the inner expression to be evaluated using the usual
GDLisp rules, which produces a list that is then inserted, as a single
element, into the surrounding list. On the other hand,

::

   `(1 2 ,.(list 3 4) 5)

This evaluates to ``(1 2 3 4 5)``, because ``unquote-spliced``
(written as ``,.``) flattens the result of evaluating the inner
expression into the outer list.

The expression within an ``unquote-spliced`` can evaluate to a list or
a Godot array. If it evaluates to any other datatype, then a runtime
error will be issued when the offending expression is interpolated.

Nested Quasiquotes
^^^^^^^^^^^^^^^^^^

.. Warning:: Nested quasiquotes are an experimental feature, whose
             behavior may change in a future version of GDLisp. Use
             with caution.

Quasiquotes can be nested. If a ``quasiquote`` appears inside of
another ``quasiquote``, then it effectively cancels off with one
``unquote`` or ``unquote-spliced`` on the inner list. That is,

::

   ``,a

This expression will not evaluate ``a`` as an expression. It will
return the constant list ``(quasiquote (unquote a))``. On the other
hand,

::

   ``,,a

This *will* evaluate the inner ``unquote`` but not the outer one, so
if ``a`` has value ``1``, then this will return ``(quasiquote (unquote
1))``.
