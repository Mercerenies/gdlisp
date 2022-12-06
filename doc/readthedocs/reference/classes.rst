
.. _classes:

Classes and Objected-Oriented Programming
=========================================

GDLisp, like GDScript, is an object-oriented programming language.
GDLisp implements a similar object inheritance mechanism to GDScript,
allowing you to define classes which inherit from exactly one
superclass and to create instances of those classes.

Classes are declared at the top-level of a module as follows. Classes
may *not* be nested within one another.

::

  (defclass ClassName (ParentClassName)
    body ...)
  (defclass ClassName (ParentClassName) public-or-private
    body ...)

The class name is a symbol literal. By convention, class names in
Godot are written in ``CamelCase``, though this is not a requirement.
The parent class is a symbol literal whose value is a class known at
compile-time. The parent class name may optionally be followed by a
:ref:`visibility modifier <visibility-modifiers>`. Class names are
placed in the value namespace, similar to constants and enums.

The parent class name may be omitted by simply replacing it with the
empty list ``()``. In this case, the parent class is assumed to be the
built-in type ``Reference``. For example, the following class
implicitly has parent class ``Reference``.

::

  (defclass MyNewReferenceType ()
    body ...)

After the class name, the class body consists of zero or more class
declarations. These are somewhat similar to normal module-level
declarations, but there are some forms that only make sense inside of
classes, and there are some subtle differences between the two, so
it's best to treat the two scopes as distinct.

Macro expansion takes place inside of class declaration scopes, just
like it does at the top level of a module.

Inside of classes, there are four valid types of declarations:
``defsignal``, ``defconst``, ``defvar``, and ``defn``. Additionally,
:ref:`progn <progn>` forms can appear in class bodies and behave
identically to the same forms at the top-level.

Class Constants
---------------

``defconst`` inside of classes works identically to the ``defconst``
top-level form. It defines a constant inside the class scope, whose
value is known at compile-time. See :ref:`constants` for more details
on how this declaration type works.

Class constants are considered "static". That is, it is not necessary
to construct an instance in order to access a class constant. Class
constants can be accessed on the class directly *or* on an instance,
and the syntax to do so uses ``access-slot`` (or, more conveniently,
the ``:`` colon syntax sugar). Given the following class, both
``Foo:A`` and ``(Foo:new):A`` will evaluate to ``42``.

::

  (defclass Foo ()
    (defconst A 42))

Class Signals
-------------

::

  (defsignal signal-name)
  (defsignal signal-name (args ...))

Signals are the only values defined in the signal namespace. A signal
is defined by ``defsignal``, followed by the name of the signal, and
then followed by a :ref:`simple lambda list <simple-lambda-lists>`. If
the argument list is omitted, then it defaults to ``()``.

Instance Variables
------------------

::

  (defvar var-name)
  (defvar var-name initial-value)

Instance variables are defined using ``defvar``. An instance variable
is a name that exists on *instances* of the class, not on the class
itself. GDLisp has no concept of "static" instance variables.

Instance variables can be accessed on instances of a class via the
``access-slot`` syntax (more conveniently written using the infix
``:`` operator). Given an instance ``my-player`` of type ``Player``,
the ``health`` field on this instance can be accessed via
``my-player:health`` (equivalently, ``(access-slot my-player
health)``).

An instance variable may optionally be followed by its initial value.
If provided, the initial value will be set at the very beginning of
the class' constructor, immediately after calling the superclass
constructor. If not provided, the initial value of the variable shall
be the null ``()`` object.

.. Note:: GDLisp, like GDScript, has no notion of static instance
          variables. All instance variables are scoped to a particular
          instance.

Initialization Time
^^^^^^^^^^^^^^^^^^^

By default, instance variables for which an initial value is given are
initialized at object construction time, after the parent ``_init`` is
called but before the current class' ``_init`` is executed. It is
often useful to initialize instance variables when a node is first
added to the scene tree. To this end, a ``defvar`` for which an
initial value is provided can optionally be succeeded by the
``onready`` symbol.

::

  (defvar var-name initial-value onready)

A variable indicated in this way will have its value set immediately
before the ``_ready`` method of the class is invoked, when the node is
added to the scene tree.

Class and Instance Functions
----------------------------

::

  (defn function-name (args ...)
    body ...)

Class-level functions are declared similarly to module-level
functions, using the ``defn`` keyword, followed by the function name,
then a list of formal arguments, and finally the function body.
Class-level functions take formal arguments as a :ref:`simple lambda
list <simple-lambda-lists>`, which means functions inside of a class
do *not* support optional or variable arguments.

A function defined inside of a class is called on instances of the
class, using an ``access-slot`` form as the head of an S-expression in
an expression context. That is, given an object ``foo``, the
expression ``(foo:bar 1 2 3)`` (or, written out in full,
``((access-slot foo bar) 1 2 3)``) will invoke the instance function
called ``bar`` on the object ``foo``, calling it with three arguments:
``1``, ``2``, and ``3``.

Inside the body of an instance function, the argument names are bound
within a local scope, similar to a module function. Additionally, the
special variable name ``self`` is bound to the instance on which the
function was invoked. The body expressions of the function are
evaluated in order, and the final expression is returned. If the
function has no body, then the null ``()`` object is returned. Like
with module functions, instance functions can be exited early with the
``return`` special form.

Static Functions
^^^^^^^^^^^^^^^^

::

  (defn function-name (args ...) static
    body ...)

A function may be marked as static by placing the keyword ``static``
keyword after the function's formal argument list. A static function
can be invoked on *either* an instance or the class itself using the
``:`` (equivalently, ``access-slot``) forms to call the function. In
either case, a static function behaves like an instance function
except that ``self`` is never bound inside the function.

.. _constructor-functions:

Constructor Functions
^^^^^^^^^^^^^^^^^^^^^

::

  (defn _init (args ...)
    body ...)

The function called ``_init`` is special. This is the function which
will be invoked when a new instance of the class should be constructed
via ``new``. Any arguments passed to the class' ``new`` function will
be forwarded onto ``_init``.

``_init`` takes a *constructor lambda list*, which permits a special
form of syntax unique to constructors, rather than a simple lambda
list. See :ref:`constructor-lambda-lists` for details. ``_init`` can
never be static. Finally, ``_init`` never returns a value. The
``return`` special form may still be used to exit the constructor
early, but its argument will be ignored.

The first expression in the body of a constructor function can be of
the form ``(super args ...)``, i.e. a proper list whose first element
is the literal name ``super``. This will cause the constructor
function to invoke the parent class' constructor with the arguments
given. This must be the first expression in a constructor function. If
an explicit ``super`` call is not supplied, then the parent class'
constructor will be called implicitly with no arguments.

.. _getter-and-setter:

Getter and Setter Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

  (defn (get field-name) ()
    body ...)
  (defn (set field-name) (arg)
    body ...)

Rather than a literal symbol, the function name of a ``defn`` can be
one of the special forms ``(get field-name)`` or ``(set field-name)``
where ``field-name`` is an arbitrary literal symbol. These define,
respectively, a getter and a setter function. Getters and setters can
never be static.

A getter function, defined with the name ``(get field-name)``, must
take zero arguments. It will be invoked when the corresponding field
(in the value namespace) is accessed on an instance of the class. That
is, a getter method ``(get health)`` will be invoked on a class
``Player`` if we have an instance of the class ``my-player`` and
attempt to access the field ``my-player:health``.

A setter function, defined with the name ``(set field-name)``, must
take exactly one argument. It will be invoked when the corresponding
field is *assigned to* with the ``set`` special form. That is, a
function ``(set health)`` will be invoked on a class ``Player`` if we
have an instance of the class ``my-player`` and write ``(set
my-player:health some-value)``. The sole argument to a setter function
is the right-hand side of the ``set`` special form. A setter function
never returns a value. Setters can be exited early with the ``return``
special form, but the value returned will be ignored.

Setters and getters for the same field may be defined on the same
class. It is an error to define an instance variable (via ``defvar``)
and a setter or a getter for the same field name on the same class.

Setters and getters are compatible with GDScript, in the sense that
attempts to access or set the field from GDScript will also trigger
the getter or setter, respectively.

Superclass Calls
^^^^^^^^^^^^^^^^

Within a non-static instance function, a special form of syntax is
available.

::

  (super:method-name args ...)

Attempting to call a method on the literal symbol ``super`` will
invoke the method of the given name on the current instance, but
considering only functions defined in the parent class or above.

This syntax only makes sense inside of instance functions in a class.
The behavior is undefined if this ``super`` call syntax is used in a
setter, getter, or constructor. ``super`` is *not* a value in the
value namespace, and it is not permitted to assign ``super`` (on its
own) to a variable or use it in some way other than the syntax shown.

Main Classes
------------

In GDScript, a single source file maps to a defined class. Functions
on a source file are, unless marked static, functions on *instances*
of that class. GDLisp works differently. A GDLisp source file is a
module, and it may *contain* one or more classes, but it is not itself
a class. These classes contained in a GDLisp source file will compile
to *inner classes* in the resulting GDScript source file.

However, there are good reasons to have control over this "top-level"
class in Godot. Packed scenes will always refer to a file's top-level
class, not to inner classes. So GDLisp provides a mechanism to define
a particular class that should be treated as the "main" class.

::

  (defclass ClassName (ParentClassName) main
    body ...)


After the class' parent name and before the class' body, the symbol
``main`` can be written to indicate that this class is the module's
"main" class. If your class has a visibility modifier, then the
``main`` modifier can be written before or after the visibility
modifier (though a private ``main`` class makes very little sense).

Designating a class as the "main" class does not change how you refer
to this class in GDLisp. It is still a class name defined on the
module in the value namespace, just like any other class, and it will
still be instantiated, imported, and used in the exact same way. The
``main`` designator does affect how the class is compiled, though.
Rather than compiling to an inner class, the main class compiles to
the top-level class of the GDScript file.

There are several limitations.

* There can be at most one ``main`` class in a file.

* There must be no conflicts between names defined inside the ``main``
  class and names defined at the module level. That is, if a constant
  is defined at the module level, then there must be no constants or
  instance variables inside the class with the same name (up to
  normalization). Likewise, if a function or macro is defined at the
  top-level, then there must be no instance functions (static or
  otherwise) with the same name (again, up to normalization).

Visibility Inside a Class
-------------------------

A class name can, like most module declarations, be declared
``public`` or ``private``. However, the elements *inside* of a class
have no visibility modifiers. There is no way to define private fields
or instance functions in GDLisp. Everything defined inside of a class
is presumed public.

Name Normalization Within Classes
---------------------------------

Name normalization works slightly differently inside of classes. The
rules for *how* names are normalized within classes are the same as at
the module level (see :ref:`name-normalization`). However, the
difference is in how names are resolved.

At the module level, a name must be referred to in the exact same way
as it was defined. That is, if you define a function called
``foo-bar``, then you must call it as ``(foo-bar ...)``. Even though,
at runtime, the resulting GDScript function will be called ``foo_bar``
(with an underscore in place of the dash), GDLisp will not allow you
to call the function as ``(foo_bar ...)``.

Inside of classes, the rules are much more lenient, owing to Godot's
dynamic nature. You may access fields or call functions on classes and
instances using *any* name that normalizes to the same name that was
used to define the function or field.

As a consequence, you can call built-in GDScript instance functions
using the conventions of GDLisp, so you can get a child node from a
node by writing ``(parent-node:get-node "ChildNodeName")``. This will
normalize to a function call to ``get_node``, which is defined by
Godot.
