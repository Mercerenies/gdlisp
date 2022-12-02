
.. _macros:

Macros
======

Macros are the key feature that separates a Lisp dialect from many
conventional programming languages. A macro is a mechanism to
dynamically generate code during compilation.

Macros are subdivided into two categories in GDLisp: functional macros
and symbol macros. Functional macros are far more common, so this
documentation will often refer to them simply as "macros" with the
"functional" part understood.

A (functional) macro declaration looks similar to a function
declaration, except that the former uses ``defmacro`` rather than
``defn``.

.. code-block::

   (defmacro macro-name (args ...)
      body ...)
   (defmacro macro-name (args ...) public-or-private
      body ...)

A macro's argument list takes the form of a :ref:`ordinary lambda list
<ordinary-lambda-lists>`, which means that macros can take optional
and variable arguments.

When a macro declaration is encountered, all names that it references,
either directly or indirectly, must always be fully defined. That is,
while two functions can mutually depend on each other, regardless of
the order in which they're defined in a file, a macro cannot depend on
a function or constant defined *later* in the current file.

Macros are defined immediately, and are available to the compiler. The
name of the macro is bound, in the function namespace, to the given
macro object for the current module. A macro is a function-like object
that is invoked during compilation.

Specifically, whenever the compiler is expecting a declaration or an
expression and encounters a proper, nonempty list ``(foo args ...)``,
it will first check whether it is aware of a macro defined in the
current scope with the name ``foo``. This process is referred to as
*macro expansion*.

If there is a macro with that name, then rather than interpreting the
code as an expression or declaration, the macro will be invoked with
the given arguments. Note carefully that the macro's arguments are
passed in *unevaluated*. That is, if there is a macro called
``example`` and it is called as ``(example arg)``, then the literal
symbol ``arg`` will be passed in, *not* the value of a variable called
``arg``. Likewise, if it is called as ``(example (+ 1 1))``, then the
literal three-element list ``(+ 1 1)`` will be passed in, not the
number ``2``.

A macro must return an S-expression, which will replace the macro call
in the current position of the code. If a macro is called in
declaration context, then the result of the macro call will be treated
as a declaration. If a macro is called in expression context, then the
result of the macro call will be treated as an expression.

The result of a macro call is itself subject to macro expansion. A
macro call can return an S-expression which itself calls another
macro, or even the same macro recursively. The behavior is undefined
if a macro exhibits infinitely recursive behavior. That is, the
following macro will exhibit undefined behavior if it is ever invoked.

::

  (defmacro recursive ()
    '(recursive))

Symbol Macros
-------------

.. code-block::

   (define-symbol-macro macro-name value)
   (define-symbol-macro macro-name value public-or-private)

The second kind of macro is a symbol macro. Symbol macros work like
functional macros except that they occupy the value namespace, not the
function namespace.

When a symbol macro is declared, it binds a name in the value
namespace of the current module. Note that symbol macros cannot take
arguments. The same caveats with regards to definedness apply for
symbol macros: all names referenced (directly or indirectly) by a
symbol macro must be fully available at compile-time, when the symbol
macro is first defined.

Macro expansion for symbol macros occurs when a symbol literal ``foo``
is evaluated in declaration or expression context. In this case,
before compiling the symbol literally, GDLisp checks whether a symbol
macro with the given name exists in the current scope. If it does,
then that symbol macro's body is evaluated in a new lexical scope, and
the return value of the symbol macro is used in place of the symbol
literal. Like functional macros, symbol macros can expand recursively
into other macro calls.

Note that the "body" of a symbol macro is a single expression, not a
collection of zero or more expressions. Symbol macros are designed to
have the same syntax as ``defconst``. To write a symbol macro whose
body consists of multiple statements, wrap the body in the ``progn``
special form.

Technical Limitations
---------------------

Macros can reference other GDLisp source files freely. However, due to
technical limitations, macros cannot currently interface directly with
GDScript source files or other resource types (such as packed scenes
or textures). This limitation may be lifted in the future.

Additionally, care must be taken if files are dynamically loaded via
the ``load`` function. GDLisp performs name mangling during macro
expansion in order to consistently load macros into the runtime. The
GDLisp compiler understands ``use`` directives and ``preload`` calls
(both of which must refer to a statically-known filename) and will
translate these names accordingly, but GDLisp will not attempt to
translate the argument to a ``load`` function. The built-in macro
``contextual-load`` can be helpful to perform such dynamic loading
inside of macros.
