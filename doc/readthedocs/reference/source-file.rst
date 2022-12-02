
Structure of a GDLisp Source File
=================================

A GDLisp source file is, by convention, written with a ``.lisp`` file
extension. GDLisp source files will be compiled into GDScript source
files (``.gd``) in a one-to-one fashion, with the resulting compiled
GDScript file taking the same filename as the input file, merely with
the extension changed. So, for example, ``Player.lisp`` would be
compiled to ``Player.gd``. GDLisp source files are *always* encoded in
UTF-8.

A GDLisp source file defines a module. Syntactically, a GDLisp source
file consists of zero or more S-expressions. With the exception of
quoted and quasiquoted terms, all lists appearing in GDLisp source
code must be *proper lists*. These top-level S-expressions will be
interpreted as declarations and will be compiled together to form a
top-level GDScript class body.

There are six types of declarations: ``defn``, ``defmacro``,
``defconst``, ``defclass``, ``defenum``, and ``define-symbol-macro``.
Additionally, there is one pseudo-declaration, called ``progn``, which
is treated specially in this context.

Namespaces
----------

Reserved Names
--------------

When choosing function or variable names, it's important to keep a few
basic rules in mind. In particular, all names that begin with ``sys/``
(including the forward slash) or ``__gdlisp`` are reserved for current
and future use by the GDLisp implementation. A programmer writing
GDLisp code should *never* define a name that starts with either of
those prefixes. Further, names defined in the ``sys/`` namespace are
strictly reserved for *internal* use and are an implementation detail
of the compiler. GDLisp programmers should never directly invoke such
functions or reference such values, and the behavior of those names
may change at any time, even in a maintenance release.

Order of Definition
-------------------

Generally speaking, classes and functions defined in a GDLisp module
can reference each other freely and can be defined in any order.
However, there is an important exception to this rule, and that is
macros.

When a macro ``foo`` is defined, whether by ``defmacro``,
``define-symbol-macro``, ``macrolet``, or ``symbol-macrolet``, that
macro and all of its dependencies must be *fully* defined. That is,
every function that ``foo`` calls and every constant, enum, or class
name that ``foo`` references must already be defined in a preloaded
file or *earlier* in the current file, and the same must be
recursively true of all of the names referenced by the macro ``foo``.
This is a very strong constraint which is necessary to allow the macro
to be loaded during compilation.
