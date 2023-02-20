
Macros
======

Macros are the centerpiece of any Lisp. Macros provide a mechanism for
modifying the code of your program using other program code. A macro
declaration looks like a function declaration but is declared using
``defmacro`` instead of ``defn``.

::

   (defmacro macro-name (args ...)
     ...)

Macros can only be declared at module scope, not inside of a class.
When a macro is *called*, its arguments are not evaluated. Instead, it
is given the syntax of the arguments directly and should return the
syntax of a new GDLisp expression to use in its place.

This is best demonstrated with some examples. ``when`` is a macro
built into GDLisp that evaluates a conditional and then runs the code
inside if (and only if) the conditional is true. Essentially, ``when``
is an ``if`` block that does not have an "else" branch and for which
the "true" case can be longer than one expression.

If GDLisp did not provide this macro, we could write it as follows::

  (defmacro when (condition &rest body)
    `(if ,condition
       (progn ,.body)))

Let's break this down, as it uses several new features. When the macro
is called, it must be called with at least one argument. The first
argument, as an abstract syntax tree, will be assigned to
``condition``, and the rest will be accumulated into a list and passed
as ``body``. Inside the macro, we use a new form of syntax called
*quasiquoting*.

Any GDLisp expression can be *quoted*. By calling the ``quote``
special form, or equivalently putting a single quotation mark ``'``
before it, evaluation of the expression is delayed. For literals like
strings or numbers, this has no effect, as strings and numbers are
self-evaluating forms. For lists and symbols, quoting the value
returns a reified representation of the value. That is, ``(if c t f)``
is an ``if`` expression that will evaluate ``c`` and then branch, but
``'(if c t f)`` evaluates to a literal list of four elements, all of
which are symbols in this example.

Quoting is useful when we have a constant expression that we wish to
reify at runtime. But often, especially with macros, we wish to have a
*mostly* constant expression with some unknown values interpolated in.
For this, we use *quasiquoting*. You can think of quasiquoting as
being sort of like string interpolation in Python or Ruby, but for
arbitrary expressions, not just strings.

A quasiquote begins with the backtick `````, which expands to the
``(quasiquote ...)`` special form. Inside the quasiquote, everything
is interpreted literally with the following two exceptions.

* ``(unquote ...)`` blocks (equivalently, a prefix comma ``,``) will
  be evaluated and spliced as an element into the result.
* ``(unquote-spliced ...)`` blocks (equivalently, a prefix ``,.``)
  will be evaluated and concatenated into the result. The enclosing
  context must be either a list or an array.

To see the difference, consider the following::

  ;; Assume the variable x has the value (2 3 4)
  `(1 x 5)   ; No unquote, evaluates to (1 x 5)
  `(1 ,x 5)  ; Regular unquote, evaluates to (1 (2 3 4) 5)
  `(1 ,.x 5) ; Spliced unquote, evaluates to (1 2 3 4 5)

Looking back at our ``when`` example::

  (defmacro when (condition &rest body)
    `(if ,condition
       (progn ,.body)))

The ``when`` macro, when called, returns an S-expression whose head is
``if``. The first argument to ``if`` is our condition. Then the second
argument is a ``progn`` which runs the entire body (as a list of
expressions) if the condition is true.

We could write its opposite, ``unless``, in a similar way::

  (defmacro unless (condition &rest body)
    `(if (not ,condition)
       (progn ,.body)))

Macros also work in declaration context, both at module scope and
inside of a class. We've already seen one such macro: ``defvars``.

::

   (defmacro defvars (&rest args)
     (let ((var-decls (list/map (lambda (name) `(defvar ,name)) args)))
       `(progn ,.var-decls)))

Note that the ``let`` block is *not* part of the expanded code. The
``let`` block is code that's run when the macro is called and is used
to *compute* the macro expansion, which is a ``progn`` consisting of
several ``defvar`` special forms in a row.

.. Note:: You may be wondering how ``progn`` works here, since it's an
          *expression* that evaluates expressions in order, while
          ``defvar`` is clearly a declaration that only makes sense at
          class or module scope.

          The answer is that ``progn`` is actually deep magic. It's
          even more special than other "special" forms, in that
          ``progn`` is the one thing in GDLisp that is valid
          fully-expanded in both expression or declaration context (or
          both, incidentally, but that can only occur in the command
          line REPL).

GDLisp provides several macros built-in, which you'll grow accustomed
to using as a matter of course (as mentioned before, ``if`` itself is
a macro, written in terms of ``cond``). For end-user games, you might
never write ``defmacro`` at all, but it's an invaluable tool for
library authors who wish to do advanced code generation. And even in a
runnable game, you might find yourself automating the boring bits of
code generation from time to time as well.
