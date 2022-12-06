
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
is treated specially in this context. Finally, ``use`` directives
appear in declaration context, despite not being declarations
themselves.

Macros are discussed in :ref:`macros`. Classes are discussed in
:ref:`classes`. ``use`` directives are discussed in :ref:`imports`.
The other declaration forms are discussed below.

Namespaces
----------

All GDLisp declarations fall into one of three *namespaces*. The
namespaces are intentionally kept distinct, and it is possible to
define the same name in all three namespaces in a given scope.

* The value namespace consists of variables and constants and their
  values. The value of a variable or constant can be a first-class
  function object (reified as a value), as well as any other type of
  object in GDLisp. Names in the value namespace are declared at the
  top-level by ``defconst``, ``defclass``, ``defenum``, and
  ``define-symbol-macro``. Names can be *locally* introduced to the
  value namespace through several special forms, most prominently
  ``let``.

* The function namespace consists of functions that have been
  explicitly bound to a function name. Functions are called by simply
  indicating the name of the function as the head of an S-expression
  in the source code. A name in the function namespace is *always*
  bound to a value of function type or to a macro. At the top-level,
  names in the function namespace are declared by ``defn`` and
  ``defmacro``. Names can be *locally* introduced to the function
  namespace through several special forms, such as ``flet`` and
  ``labels``.

* The final namespace is the signal namespace. This namespace only
  exists inside the scope of a class declaration, never at the
  top-level of a module. The signal namespace is the namespace which
  contains signals *within* a class defined by ``defsignal``. Classes
  are discussed later.

Functions
---------

.. code-block::

   (defn function-name (args ...)
      body ...)
   (defn function-name (args ...) public-or-private
      body ...)

A function is the fundamental unit of code execution in GDLisp. A
function defined at the top-level of a module compiles to a static
function in the resulting GDScript file.

A function declaration introduces a name into the function namespace
for the current module. When the function is called, the arguments
will be bound to the formal arguments of the function in a new lexical
scope, and the body of the function will be executed, in order, in
that lexical scope. The final expression of the function body is
always returned from the function, but it is also possible to return
early using the ``return`` special form. If a function body is empty,
then the function is a no-op which silently returns the null object
``()``.

The list of formal arguments is an :ref:`ordinary lambda list
<ordinary-lambda-lists>`. When a function is called, the number of
arguments passed to the function is validated at compile-time, and an
error is issued if the count is incompatible with the function's
lambda list.

Compilation of Functions
^^^^^^^^^^^^^^^^^^^^^^^^

A GDLisp function translates to a GDScript function with its name
:ref:`normalized <name-normalization>`. No guarantees are currently
made about the GDScript signature of the resulting function if
optional or variable arguments are used. However, if the function only
accepts *required* arguments, then the resulting function is
guaranteed to compile to a function that accepts exactly the same
number of required arguments. Therefore, if you intend to call a
GDLisp function from GDScript, then the GDLisp function should take
only required arguments, in order to maximize compatibility.

.. _constants:

Constants
---------

.. code-block::

   (defconst constant-name constant-value)
   (defconst constant-name constant-value public-or-private)

A constant is a top-level immutable name which binds to a value. The
value of a constant must be an expression whose value is known at
compile time.

The notion of a *constant expression* is recursively defined as

* The name of another constant.

* A literal value. (Due to Godot limitations, it is not currently
  possible to assign *symbol* literals to constants. This limitation
  will hopefully be lifted in a future version of GDLisp.)

* A ``progn`` block with zero terms, or a ``progn`` block with exactly
  one term which is a constant expression.

* A built-in function call that performs basic arithmetic, such as
  ``+`` or ``abs``, which is called with only constant expressions as
  arguments.

* An array or dictionary literal consisting of only constant
  expressions as elements.

* A field access ``foo:bar``, where ``foo`` is the name of an enum and
  ``bar`` is a valid option for that enum.

* A call to the ``preload`` special form.

A constant declaration defines a name in the value namespace for the
current module.

Enumerations
------------

.. code-block::

   (defenum enum-name entries ...)
   (defenum enum-name public-or-private entries ...)

An enumeration is a scoped namespace containing a finite number of
values, useful for representing a collection of choices.

Each entry in the enumeration can be specified either as a symbol
literal or as a two-element list, where the first element is the
symbol literal and the second is the constant expression indicating
the value of the enum entry.

Examples::

   (defenum PlayerChoice
      ATTACK DEFEND HEAL PASS)

   (defenum Color
     (RED "Red") (GREEN "Green") (BLUE "Blue"))

An enumeration defines a name in the value namespace. This name
behaves like a GDLisp object and has fields defined corresponding to
the names indicated in the entries. In the first example above,
``PlayerChoice`` is a value for which ``PlayerChoice:ATTACK``,
``PlayerChoice:DEFEND``, ``PlayerChoice:HEAL``, and
``PlayerChoice:PASS`` are all distinct integer values. In the second
example above, ``Color`` is a value, and ``Color:RED`` is the string
``"Red"``, ``Color:GREEN`` is the string ``"Green"``, and
``Color:BLUE`` is the string ``"Blue"``.

.. _progn:

The ``progn`` Directive
-----------------------

.. code-block::

   (progn body ...)

``progn`` is a special sort of directive, in that it can be used as a
declaration *or* an expression. In declaration context, it takes zero
or more declarations and evaluates them in order in the *current*
scope, as though the ``progn`` wasn't even there.

A ``progn`` directive is never useful directly in a file in
declaration context, since it would be easier and more readable to
simply place the declarations at the top-level. However, it is useful
in macro expansions, when a macro wishes to define multiple
declarations but must evaluate to a *single* declaration S-expression.

.. _visibility-modifiers:

Visibility Modifiers
--------------------

Several module-level declarations take an optional visibility
modifier. A visibility modifier, if provided, must either be the
symbol ``private`` or the symbol ``public``. If a visibility modifier
is not provided, it is always assumed to be ``public``.

A name with public visibility can be accessed from anywhere. Any other
module is free to import the name and reference, call, or instantiate
it at their liberty.

A name with private visibility is only directly usable within the
current module. The current module can freely use the name, but it is
an error to attempt to import the name in *another* module.

.. _name-normalization:

Name Normalization
------------------

GDLisp is far more lenient than GDScript when it comes to identifiers.
In particular, GDLisp allows several non-standard characters such as
``-`` and ``?`` in identifiers, as well as Unicode characters.
Additionally, GDLisp does not have a notion of "keywords", and it's
perfectly kosher to define a variable called ``if`` or ``while``
(though it may confuse the readers of your code).

When the GDLisp compiler translates your code into GDScript, it must
convert these identifiers into valid GDScript identifiers. The exact
translation rules are an implementation detail that may change in
future releases of GDLisp, but some guarantees are made in order to
maximize compatibility.

* Any name which is a valid GDScript identifier and *not* a GDScript
  keyword will be left unchanged. So ``foo``, ``foobar``,
  ``player_health1``, and ``i`` will all be left untouched by the
  GDLisp compiler.

* An ASCII arrow ``->`` in a name will be translated to ``_to_``. This
  allows a name like ``array->list`` to translate into GDScript as
  ``array_to_list``.

* A dash ``-`` that is *not* part of an ASCII arrow is translated to
  an underscore ``_``. So a conventional Lisp function like
  ``create-player`` will have its name translated into the GDScript
  function ``create_player``.

* A question mark ``?`` at the *end* of a name will be translated to
  ``is_`` at the beginning. For instance, a Lisp predicate called
  ``positive-number?`` will be translated to the GDScript function
  ``is_positive_number``.

* If the name is a reserved word in GDScript, then an underscore will
  be prefixed, so ``if`` translates to ``_if`` when used as a variable
  name.

* Any other characters, or a ``?`` that is not at the end of a name,
  will translate in an implementation-defined way.

.. Warning:: It is undefined behavior to define two names in the same
   scope and namespace that will normalize to the same name under
   these rules. So, for example, it is undefined behavior to define
   functions called ``foo-bar`` and ``foo_bar`` in the same scope,
   since these names will both translate to ``foo_bar``.

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
