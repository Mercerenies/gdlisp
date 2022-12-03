
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

Array Literals
--------------

Variable Names
--------------
