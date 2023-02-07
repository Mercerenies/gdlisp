
Built-in Macros
===============

All of the global macros available in GDLisp are documented here.

``->``
------

::

   (defmacro -> (arg &rest forms)
     ...)

First-argument threading. Evaluates ``arg``. Then, for each form in
``forms``, passes the accumulated argument as the first argument to
the given list (shifting the other arguments over). Any argument which
is a symbol is treated as a one-element list.

Examples::

  (-> a b c) ; Expands to (c (b a))
  (-> a (b 1) c (d 2 3)) ; Expands to (d (c (b a 1)) 2 3)

``->>``
-------

::

   (defmacro ->> (arg &rest forms)
     ...)

Last-argument threading. Evaluates ``arg``. Then, for each form in
``forms``, passes the accumulated argument as the last argument to the
given list. Any argument which is a symbol is treated as a one-element
list.

Examples::

  (->> a b c) ; Expands to (c (b a))
  (->> a (b 1) c (d 2 3)) ; Expands to (d 2 3 (c (b 1 a)))

``and``
-------

::

   (defmacro and (&rest args)
     ...)

Short-circuiting conjunction operator. Returns the first falsy value
out of its arguments, evaluating as few arguments as necessary to do
so. Returns ``#t`` if given no arguments.

``as->``
--------

::

   (defmacro as-> (arg var &rest forms)
     ...)

Arbitrary threading macro. Evaluates ``arg``. Then evaluates the first
of ``forms`` with ``var`` bound to ``arg``. The next form in ``forms``
is evaluated with the result of the prior bound to ``var``, and so on.
The result of the final form is returned.

Example::

  ;; Equivalent:
  (as-> (foo) v (bar 2 v 3) (baz 1 v 4 5))
  (baz 1 (bar 2 (foo) 3) 4 5)

``contextual-load``
-------------------

::

   (defmacro contextual-load (filename)
     ...)

Expands to a ``load`` call to the file with the name ``filename``.
However, unlike a plain ``load`` call, ``contextual-load`` will be
understood during macro expansion and translated to an appropriate
virtual filename. Thus, ``load`` should not be used directly in macro
bodies, as it will fail when run during macro expansion.

.. Caution:: While the GDLisp macro engine understands how to
             translate names into virtual filenames inside a
             ``contextual-load``, it will NOT attempt to trace
             dependencies into the corresponding file. So
             ``contextual-load`` is *only* safe to use on a filename
             which has already been loaded in the current scope. For
             instance, ``(contextual-load (this-true-filename))`` is
             always safe, since the current file is always (trivially)
             loaded.

``deflazy``
-----------

::

   (defmacro deflazy (name value &rest modifiers)
     ...)

``deflazy`` defines the name ``name`` in the value namespace of the
current scope. That name, when it is evaluated for the *first* time,
will evaluate ``value`` and return it. The value will then be cached
and returned when ``name`` is evaluated in the future.

The only valid modifiers are ``public`` and ``private``, which set the
visibility of the defined name. New modifiers may be added in the
future.

``defobject``
-------------

::

   (defmacro defobject (name parent &opt visibility &rest body)
     ...)

Defines a new subclass of ``parent``, whose visibility is
``visibility`` (or public, if not provided) and whose class body is
``body``. This subclass only has one instance, which is lazily
initialized to the name ``name``. The first time ``name`` is
evaluated, the instance is constructed and returned. On future
evaluations of ``name``, the same instance will be returned.

``defvars``
-----------

::

   (defmacro defvars (&rest args)
     ...)

``defvars`` expands into multiple ``defvar`` blocks with no
initializers. That is, ``(defvars a b c)`` is equivalent to

::

   (defvar a)
   (defvar b)
   (defvar c)

There is no way to include initializers or ``onready`` modifiers with
this macro. For such use cases, call ``defvar`` directly.

.. _macro-if:

``if``
------

::

   (defmacro if (cond true-case &opt false-case)
     ...)

Evaluates ``cond``. If it's true, then evalautes and returns
``true-case``. If ``cond`` is false, then evaluates and returns
``false-case``. If not provided, ``false-case`` defaults to ``()``.

``let*``
--------

::

   (defmacro let* (vars &rest body)
     ...)

``let*`` is equivalent to ``let`` except that each variable clause in
a ``let*`` is evaluated in sequence and has access to the variables
declared before it in the same ``let*`` block. That is,

::

   (let* ((a 1)
          (b (+ a 1)))
     b)

is equivalent to

::

   (let ((a 1))
     (let ((b (+ a 1)))
       b))

and will return ``2``. Attempting to do the same with a single ``let``
block will result in the ``a`` variable not being in scope during
initialization of ``b``.

``list/for``
------------

::

   (defmacro list/for (var list &rest body)
     ...)

Equivalent to the ``for`` special form, but works on lists rather than
arrays.

``var`` is a symbol name and ``list`` is an expression that evaluates
to a list. ``list`` is evaluated, and then ``body`` is run once per
list element in a local scope where ``var`` is defined to be the
current list element. Always returns ``()``.

``or``
------

::

   (defmacro or (&rest args)
     ...)

Short-circuiting disjunction operator. Returns the first truthy value
out of its arguments, evaluating as few arguments as necessary to do
so. Returns ``#f`` if given no arguments.

``quit``
--------

::

   (defmacro quit ()
     ...)

Expands to a call to the ``quit`` method on the scene tree. This is
most commonly used in the REPL, where ``(quit)`` will quickly and
easily exit the REPL.

``this-file``
-------------

::

   (defmacro this-file ()
     ...)

``(this-file)`` is an expression which will load the current file.
``this-file`` can be used in macro contexts to dynamically load the
current file. Equivalent to ``(load (this-filename))``.

``this-filename``
-----------------

::

   (defmacro this-filename ()
     ...)

``(this-filename)`` evaluates to a string consisting of the path to
the current file being compiled (as a ``.gd`` file). In macro
expansion, ``(this-filename)`` will expand to the virtual filename of
the file, suitable to be used *during* macro expansion.

``this-true-filename``
----------------------

::

   (defmacro this-true-filename ()
     ...)

``(this-true-filename)`` evaluates to a string consisting of the path
to the current file being compiled (as a ``.gd`` file). In macro
expansion, ``(this-true-filename)`` will expand to the *real* runtime
filename of the file. This filename is *not* suitable to load during
macro expansion but it should be used in situations where a macro is
attempting to expand *into* a ``load`` call which will happen at
runtime.

.. _macro-unless:

``unless``
----------

::

   (defmacro unless (cond &rest body)
     ...)

Shorthand syntax for an ``if`` block which *only* has an ``else``
part. If ``cond`` is false, evaluates and returns the body. If
``cond`` is true, returns ``()``.

``update``
----------

::

   (defmacro update (field updater)
     ...)

Convenient shorthand for updating a field. ``(update x (foo y z))``
expands to ``(set x (foo x y z))``. That is, the value ``field`` is
taken and passed as the first argument to the ``updater`` (with the
other arguments, if any, shifted one to the right), and then the
result is put back into the place ``x``. ``x`` can be anything that is
valid on the left-hand side of a ``set``. See :ref:`expr-set` for more
details on the valid argument forms.

If ``updater`` is a symbol rather than a proper list, then it is
wrapped in a one-element list before expanding.

Examples::

  (update a (+ 1)) ; Adds 1 to the variable a
  (update b -) ; Sets b equal to its negative
  (update player:position (* 2)) ; Multiplies the position field on player by 2
  (update (elt x 0) (/ 2)) ; Divides the first element of the array x by 2

.. _macro-when:

``when``
--------

::

   (defmacro when (cond &rest body)
     ...)

Shorthand syntax for an ``if`` block which has no ``else`` part. If
``cond`` is true, evaluates and returns the body. If ``cond`` is
false, returns ``()``.

``yield*``
----------

::

   (defmacro yield* (arg)
     ...)

If ``arg`` is a function call which yields and produces a
``GDScriptFunctionState`` object, then this macro yields the *current*
function as well. When the current function is resumed, then so too
shall the inner function. When the inner function terminates and
returns normally, the result of the inner function is returned from
``yield*``. Effectively, ``yield*`` propagates a ``yield`` from
``arg`` to the caller.

Example::

  (defn foo ()
    (print 2)
    (yield)
    (print 4)
    (yield)
    (print 6))

  (defn bar ()
    (print 1)
    (yield* (foo))
    (print 7))

  (let ((x (bar)))
    (print 3)
    (set x (x:resume))
    (print 5)
    (x:resume)
    (print 8))

This code will print the numbers from 1 to 8 in order. Note that the
actual "yielding" is done in ``foo``, but when we resume from our
``let`` block, we resume *through* the ``bar`` function.

.. Warning:: ``yield*`` should only be used to yield from functions
             that used the 0-argument form of ``yield``. If a function
             uses the 2-argument ``yield`` and then is resumed by a
             signal, the intermediate ``yield*`` object will be left
             in an un-resumable state.
