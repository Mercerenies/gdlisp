
Classes in GDLisp
=================

GDLisp prides itself on being a functional language, capable of
manipulating functions as first-class objects and providing powerful
abstraction techniques to support common functional paradigms. But
GDLisp targets the Godot platform, which is designed with an
object-oriented paradigm in mind. GDLisp fully supports this paradigm
and provides mechanisms for working with classes, both those built-in
to Godot and those from GDScript source files, as well as for
declaring new classes in GDLisp proper.

Using Classes
-------------

Every built-in GDScript class is available in GDLisp as a global name.
This includes classes deriving from ``Object``, like ``Node``,
``Spatial``, and ``Control``, as well as typenames that are considered
primitive in Godot, such as ``Int`` and ``Vector2``.

To access a field on a class or an object, put a colon ``:`` after the
class or object name and follow it by the name of the constant or
variable. For example::

  Vector2:ZERO
  Transform:FLIP_X
  Quat:IDENTITY
  some-timer-node:paused
  some-material:render_priority

Likewise, to call a method on an object or a class, use the colon
syntax as the first term of an S-expression.

::

   (Reference:new)
   (Engine:get_singleton "MySingleton")
   (some-node:get_path)
   (my-object:free)

Declaring Classes
-----------------

Classes can be declared inside a module using the following syntax.

::

   (defclass ClassName (ParentClassName)
     body ...)

Note that ``ParentClassName`` must always be a (possibly qualified)
class *name*. Unlike GDScript, GDLisp does not allow string paths to
be used as the name of a parent class.

As with functions and other declarations, ``defclass`` can be suffixed
with the symbol ``private`` to make the class invisible outside of the
current module.

::

   (defclass ClassName (ParentClassName) private
     body ...)

Inside the class, several types of declarations are supported. Note
that declarations inside a class are always public, as GDLisp cannot
enforce access restrictions on class members.

Signal Declarations
^^^^^^^^^^^^^^^^^^^

::

   (defsignal signal_name (args ...))

The ``defsignal`` form declares the existence of a signal with the
given name and argument list. Note that signals are conventionally
named in ``snake_case``, not ``train-case``, for maximum compatibility
with Godot signals. If the argument list is empty, it can be omitted.

The argument list, if provided, must be a :ref:`simple lambda list
<simple-lambda-lists>`. That means that the ``&opt``, ``&rest``, and
``&arr`` directives are not supported.

Constant Declarations
^^^^^^^^^^^^^^^^^^^^^

::

   (defconst ConstantName value)

Constants inside a class work identically to those at the
module-level, with the exception that a class-scoped ``defconst``
cannot be marked ``private``. Note that ``defconst`` is evaluated at
class scope, so ``self`` does *not* exist in the right-hand side of a
``defconst``.

Variable Declarations
^^^^^^^^^^^^^^^^^^^^^

::

   (defvar var-name initial-value)

Variables declared inside a class function as instance variables. When
an instance of the class is initialized, it will receive an instance
variable with the given name. ``initial-value`` is optional, but if
provided it will be used as the initial value of the instance variable
at initialization time. ``initial-value`` can contain arbitrary
expressions and can use ``self``.

::

   (defvar var-name initial-value onready)

If the ``defvar`` form is followed by the ``onready`` keyword, then
the initial value will be applied during the ``_ready`` method (when
the node is added to the tree) instead of the ``_init`` method (when
the object is constructed). The ``onready`` modifier makes no sense on
objects that are not nodes.

.. Note:: If you're wondering where the GDScript ``setget`` keyword
          is, GDLisp doesn't have ``setget``. GDLisp implements proxy
          properties in a different way. See
          :ref:`getters-and-setters` below.

Export Modifiers
""""""""""""""""

A variable declaration in a class can be followed by an ``export``
form, which will be compiled into a GDScript ``export`` clause. This
provides certain information to the Godot editor indicating what
values are acceptable for the instance variable.

Examples::

  (defvar player-hp 10 (export int))
  (defvar character-name "Alice" (export String "Alice" "Bob" "Charlie"))
  (defvar background-color Color:red (export Color))
  (defvar player-node (export NodePath))

The ``defvars`` Macro
"""""""""""""""""""""

If you're simply declaring several instance variables in a row and do
not need to provide initial values for any of them, you may use the
:ref:`macro-defvars` macro. ``defvars`` takes any number of instance
variable names and declares those variables, without initial values.
``defvars`` does not support ``export`` or ``onready``.

::

   (defvars player-hp character-name background-color)

Instance Functions
^^^^^^^^^^^^^^^^^^

The main lifeblood of a class is its instance methods, which are
declared using a similar ``defn`` syntax to module functions.

::

   (defn method-name (args ...)
     body ...)

Like signal declarations, instance function declarations take a simple
lambda list, which means modifiers such as ``&opt``, ``&arr``, and
``&rest`` are not allowed in this context.

Inside the instance method body, the variable ``self`` is available
and refers to the current instance of the class. Note that GDLisp does
*not* implicitly insert ``self`` in any context. That is, a bare name
like ``example`` will *always* refer to a statically-scoped local
variable or module constant with the name ``example``, even inside a
class. To refer to the instance variable with that name, you must
explicitly write ``self:example``. The syntax sugar ``@example`` is
provided, which desugars to ``self:example``.

Methods may be marked ``static``, to indicate that they should be
called on the class itself rather than an instance.

::

   (defn method-name (args ...) static
     body ...)

Inside a static method, the name ``self`` is *not* bound.

.. Tip:: You probably shouldn't be using the ``static`` modifier very
         often. Usually, when you find yourself writing a ``static``
         class method, that method would be better written as a
         top-level module function instead. ``static`` is provided
         mainly for compatibility with GDScript.

Super Calls
"""""""""""

Inside an instance method, you may use the special syntax
``(super:method-name ...)`` to invoke the method called
``method-name`` on the *superclass* of the current class.
``method-name`` need not be the name of the currently-executing method
(though it usually will be, in practice). Note that ``super`` on its
own is *not* a variable, so attempting to assign ``super`` to a local
variable or pass it as a function argument will fail.

Constructors
""""""""""""

Class constructors in GDLisp are a bit special and have some
additional syntax to accommodate that. A constructor is called
``_init`` and is declared using ``defn`` like any other instance
function. Constructors do not return values, though ``(return nil)``
can still be used to exit the constructor early.

::

   (defn _init (args ...)
     body ...)

A class constructor cannot be made ``static``. Inside the body of the
constructor, if you wish to call the superclass' constructor, you may
do so by calling the function ``super`` as the *first* expression in
the constructor.

::

   (defn _init (args ...)
     (super args ...)
     body ...)

The ``super`` call, if present, *must* be the first expression in the
constructor body.

The argument list to a constructor supports a special syntax unique to
constructors. An ``@`` sign can be placed before the name of an
argument.

::

   (defn _init (@foo @bar))

In this case, ``foo`` and ``bar`` are *not* local variables to the
constructor. Instead, the values given to those parameters are
assigned directly to instance variables on the class itself.
Essentially, the above is equivalent to

::

   (defn _init (foo bar)
     (set @foo foo)
     (set @bar bar))

Note that the automatic assignment happens after any ``super``
constructor call.

.. _getters-and-setters:

Getters and Setters
"""""""""""""""""""

Getters and setters are special instance methods that look like
ordinary instance variables, from a caller's perspective.

Getters are declared using ``defn`` with a special method name of the
form ``(get ...)``. A getter method must be non-static and cannot take
any arguments.

::

   (defn (get variable-name) ()
     body ...)

When a user of the class attempts to get the instance variable
``variable-name`` from the class, the method ``(get variable-name)``
will be called instead, and its result will be used as the value of
the expression.

Likewise, setters are declared using a ``(set ...)`` name. A setter
method must be non-static and must take exactly one argument. Like a
constructor, a setter does not return any values, though it can exit
early with ``(return nil)``.

::

   (defn (set variable-name) (value)
     body ...)

The setter will be invoked when a caller attempts to ``set`` the given
instance variable.

::

   (set my-instance:variable-name value)

The same variable name can be used for a setter and a getter. The name
of a setter/getter cannot coincide with the name of an actual,
concrete ``defvar`` instance variable on the class. Setters and
getters are fully compatible with GDScript, in that a caller from
GDScript who attempts to get or set the given variable will correctly
invoke the getter or setter function, respectively.

If your goal is to wrap a ``defvar`` with some validation or some
actions, a common idiom is to precede the ``defvar`` with an
underscore and then use the property outside the class.

Example::

  (defclass Player (Node2D)
    (defsignal hp_changed)

    (defvar _hp 10)
    (defvar max_hp 10)

    (defn (get hp) ()
      ;; Simply return the instance variable
      @_hp)

    (defn (set hp) (x)
      ;; Make sure the HP value is in bounds
      (set @_hp (clamp x 0 @max_hp))
      ;; Let everyone know the value has changed
      (@emit_signal "hp_changed")))

Main Classes
------------

Godot is based around the idea that every file is also a class, in
some form or another. In GDLisp, this is not the case. However, it is
often useful to designate one class in a GDLisp module as the
"primary" class of that module, so that Godot can link scenes and
other resources up to it in the editor.

For this reason, GDLisp classes can be marked with the ``main``
modifier.

::

   (defclass ClassName (ParentClassName) main
     body ...)

Every file can have at most one ``main`` class, and the ``main`` class
cannot be private. From the perspective of GDLisp modules, the
``main`` modifier changes nothing. A GDLisp module will still import
the class name with ``use`` just like any other name. However, in the
resulting GDScript file, the ``main`` class will be compiled to the
top-level class of the file, rather than a nested class. This allows
the editor to bind the ``main`` class to a packed scene or other
resource.

Signals
-------

Godot communicates between nodes and other objects using signals. For
the most part, GDLisp supports signals and connections just like
GDScript::

  (my-object:connect "signal" target-object "_target_method_name")
  (my-object:disconnect "signal" target-object "_target_method_name")

However, GDLisp also offers some convenience functions to deal with
common use cases.

::

   (connect>> my-object "signal" (lambda () ...))

The built-in function ``connect>>`` connects a signal to a lambda
function. When the signal is fired, the lambda function will be called
and can accept the arguments of the signal. As a first-class function,
the lambda is free to close around any local variables (including
``self``, if applicable) in the current scope, so you should never
have to explicitly bind variables of a signal connection when using
``connect>>``.

GDLisp also provides a function ``connect1>>``, which takes the same
arguments but connects a signal as if using the one-shot flag. That
is, the connection removes itself after firing once.

Both ``connect>>`` and ``connect1>>`` return a unique value to
identify the connection. To disconnect a signal that was connected in
this way, use ``disconnect>>``, which takes the return value of
``connect>>`` or ``connect1>>`` and disconnects it.

.. Note:: You may freely intermix GDLisp-style ``connect>>`` calls and
          Godot ``connect`` method calls in the same program. However,
          ``disconnect>>`` should only be used on connections
          established with ``connect>>`` or ``connect1>>``, while the
          built-in Godot method ``disconnect`` should be used for
          those established with ``connect`` or through the Godot UI.
