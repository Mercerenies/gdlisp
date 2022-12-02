
Basic Datatypes
===============

GDLisp compiles to GDScript and runs on the Godot engine, so the core
datatypes for GDLisp are the same as those of GDScript. That is,
GDLisp defines the following core types, identical to their
representations in Godot.

For more details about these basic types, see the `GDScript
documentation
<https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_basics.html>`_.
They are listed here for completeness.

* A ``Null`` type with a single value. The single value of this type
  is written as ``()``, or ``nil``. (Note that ``()`` is the literal
  representation of the null value, whereas ``nil`` is merely a global
  constant which is *defined* to be ``()``)

* A ``Bool`` type with two values, written as ``#f`` for false and
  ``#t`` for true.

* An ``Int`` type for 64-bit signed integers.

* A ``Float`` type representing double-precision floating point
  values.

* A ``String`` type consisting of UTF-8 strings.

* The ``Vector2`` and ``Vector3`` types, with literals written
  enclosed in ``V{`` and ``}``. Both types of vectors are written in
  this way; they are distinguished by the number of arguments to the
  syntactic form.

* ``Rect2``, ``Transform2D``, ``Plane``, ``Quat``, ``AABB``,
  ``Basis``, ``Transform``, ``Color``, ``NodePath``, and ``RID`` all
  behave identically to GDScript. There is no literal syntax for any
  of these types.

Cons Cells
----------

GDLisp defines a ``Cons`` datatype. ``Cons`` values are constructed
using the built-in function ``cons`` and store two fields: a ``car``
and a ``cdr``, both accessible by public field access on the cell.

Cons cells are used to build up singly-linked lists to represent the
abstract syntax tree of your code during macro expansion.

Arrays
------

GDLisp supports all of the built-in Godot array types, including the
base ``Array`` and all of its strongly-typed companions. Array
literals are written in square brackets ``[`` and ``]``, and array
elements are *not* separated by commas.

Dictionaries
------------

GDLisp supports the built-in Godot ``Dictionary`` type, which maps
keys to values. Dictionaries are written in curly braces ``{`` and
``}``. The elements of a dictionary are *not* delimited by colons or
commas; they are merely written side by side. For example, ``{key1
value1 key2 value2}``.

Functions
---------

``Function`` is the type of first class functions in GDLisp. Functions
are defined using top-level declaration forms such as ``defn`` or by
the ``lambda`` or ``function`` special forms. Calling functions is
done with the built-in ``funcall`` or its fully general companion
``apply``.

Cells
-----

A ``Cell`` is a simple type that has a single public field:
``contents``. A cell is constructed directly by the ``Cell``
constructor (``(Cell:new value)``). Cells are seldom used directly in
GDLisp code but are frequently used behind the scenes to implement
closures.

Symbols
-------

The ``Symbol`` data type represents atomic symbols in GDLisp. There is
no equivalent to this in base Godot (though it is similar in principle
to the Godot 4 type ``StringName``). A ``Symbol`` has a textual
representation, similar to a string. Symbols are most commonly
constructed by simply quoting a symbol literal, though they can also
be constructed from strings using ``intern``, and unique, unused
symbols can be constructed with ``gensym``.

The Object Hierarchy
--------------------

Like GDScript, GDLisp supports an object-oriented style of
programming. GDLisp expands upon Godot's built-in inheritance
hierarchy, introducing a partial ordering on all types.

.. mermaid::

   graph BT
       Nothing-->Symbol-->Reference-->Object-->AnyRef-->Any
       Nothing-->Node-->Object
       Nothing-->String-->AnyVal-->Any
       Nothing-->Int-->Number-->AnyVal
       Nothing-->Float-->Number
       Nothing-->Array-->BaseArray-->AnyVal
       Nothing-->Pool...Array-->BaseArray
   Nothing-->Prim["Primitive types"]-->AnyVal

At the top of the hierarchy is the ``Any`` type, which contains all
GDLisp objects, whether they inherit from the ``Object`` class or are
primitives.

Below that, the two types ``AnyRef`` and ``AnyVal`` partition the
space of objects into two. ``AnyRef`` contains all of the object types
and ``AnyVal`` contains all of the Godot primitives.

On the ``AnyVal`` side, all of the primitive Godot types are provided
as-is as subtypes of ``AnyVal``. Additionally, two new types are
provided. ``Number`` is the lowest common supertype of ``Int`` and
``Float``, and ``BaseArray`` is a supertype of all of the array types
in Godot (the generic ``Array`` as well as all of the strongly-typed
"Pool" array types).

Below ``AnyRef`` is ``Object``, the root of the Godot object
hierarchy. In the current implementation of GDLisp, ``AnyRef`` and
``Object`` are equivalent as types, but this may change in the future
if an alternative object hierarchy is added.

Below ``Object``, all of the class types available in Godot fit into
the hierarchy in the same way they do in GDScript. In particular,
user-defined classes can subclass ``Object``, ``Node``, ``Reference``,
or any of the other types freely.

Finally, ``Nothing`` is the unique bottom type of the hierarchy.
``Nothing`` is a subtype of *every* type, and there is no value which
is a member of the ``Nothing`` type. Note carefully: Not even the
special null value ``()`` is a member of the ``Nothing`` type;
``Nothing`` is uninhabited *by design*.
