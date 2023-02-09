
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


