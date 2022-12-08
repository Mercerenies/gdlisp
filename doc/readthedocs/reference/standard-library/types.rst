
Types and Classes
=================

These are all of the class and enumeration constants defined at the
top-level in GDLisp.

All of `the classes defined by Godot
<https://docs.godotengine.org/en/stable/classes/index.html>`_ are
available in GDLisp. Any such classes not listed below are available
without modification.

Godot classes that are singletons use the following convention: The
singleton object is available in GDLisp as-is, and the type that
represents it is available preceded with an underscore. So, for
example, ``Engine`` is a global constant in GDLisp that refers to the
singleton object whose type is ``_Engine``.

Godot Enumerations
------------------

The global enumerations defined in `Enumerations
<https://docs.godotengine.org/en/stable/classes/class_%40globalscope.html#enumerations>`_
have been included with modifications. The common prefix for each
enumeration type has been removed from the name of the constant and
replaced with an appropriate typename. So, for example, the name
``KEY_SHIFT``, which refers to the shift key, is referred to in GDLisp
as ``Key:SHIFT``. The following name translations take place

* ``BUTTON_`` constants belong in the ``Mouse`` enum.
* ``CORNER_`` constants belong in the ``Corner`` enum.
* ``ERR_`` constants (and the ``OK`` and ``FAILED`` constants) belong
  in the ``Err`` enum.
* ``HALIGN_`` constants belong in the ``HAlign`` enum.
* ``JOY_`` constants belong in the ``Joy`` enum.
* ``KEY_`` constants belong in the ``Key`` enum.
* ``KEY_MASK_`` constants belong in the ``KeyMask`` enum.
* ``MARGIN_`` constants belong in the ``Margin`` enum.
* ``METHOD_FLAG_`` constants (and the ``METHOD_FLAGS_DEFAULT``
  constant) belong in the ``MethodFlag`` enum.
* ``MIDI_MESSAGE_`` constants belong in the ``MidiMessage`` enum.
* ``OP_`` constants belong in the ``Op`` enum.
* ``PROPERTY_HINT_`` constants belong in the ``PropertyHint`` enum.
* ``PROPERTY_USAGE_`` constants belong in the ``PropertyUsage`` enum.
* ``TYPE_`` constants belong in the ``Type`` enum.
* ``VALIGN_`` constants belong in the ``VAlign`` enum.
* ``HORIZONTAL`` and ``VERTICAL`` go in their own enum called
  ``Orientation``.

Godot Primitive Types
---------------------

The 27 Godot primitive types are also available in GDLisp. In GDLisp,
these types are real type objects and can be used as the right-hand
side of an ``instance?`` call. Since they are real objects in the
GDLisp ecosystem, they can also be assigned to variables and passed as
function arguments. Additionally, the names of the GDLisp primitive
types are normalized to be consistent with other class names.
Specifically,

* ``Null`` refers to the Godot type whose constant is ``TYPE_NIL`` and
  whose only value is the null object ``()``.

* ``Bool`` refers to the Godot type ``bool`` whose constant is
  ``TYPE_BOOL``.

* ``Int`` refers to the Godot type ``int`` whose constant is
  ``TYPE_INT``.

* ``Float`` refers to the Godot type ``float`` whose constant is
  ``TYPE_REAL``.

* ``String``, ``Vector2``, ``Rect2``, ``Vector3``, ``Transform2D``,
  ``Plane``, ``Quat``, ``AABB``, ``Basis``, ``Transform``, ``Color``,
  ``NodePath``, ``RID``, ``Object``, ``Dictionary``, ``Array``,
  ``PoolByteArray``, ``PoolIntArray``, ``PoolStringArray``,
  ``PoolVector2Array``, ``PoolVector3Array``, and ``PoolColorArray``
  are available in GDLisp and refer to the Godot primitive types of
  the same name.

``_GDLisp``
-----------

::

   (defclass _GDLisp (Node)
     ...)

The class of the singleton GDLisp support object. The behavior of the
program is undefined if an attempt is made to construct any additional
instances of this type.

``Any``
-------

::

   (defclass Any (<no-parent>)
     ...)

The type of all values in GDLisp. Every single value is an value of
this type.

``AnyRef``
----------

::

   (defclass AnyRef (Any)
     ...)

The type of all object values in GDLisp.

``AnyVal``
----------

::

   (defclass AnyVal (<no-parent>)
     ...)

The type of all primitive values in GDLisp. All values which are not
instances of ``AnyRef`` are instances of ``AnyVal``.

``BaseArray``

::

   (defclass BaseArray (AnyVal)
     ...)

The common supertype of all array types in GDLisp, including ``Array``
itself and the seven strictly-typed pool array types.

``ConnectFlags``
----------------

::

   (defenum ConnectFlags
     (DEFERRED ...)
     (PERSIST ...)
     (ONESHOT ...)
     (REFERENCE_COUNTED ...))

This enumeration makes global the `ConnectFlags enumeration
<https://docs.godotengine.org/en/stable/classes/class_object.html#enumerations>`_
in Godot.

``Nothing``
-----------

::

   (defclass Nothing (<all-parents>)
     ...)

The bottom of the type hierarchy. There are not, and never will be,
values of this type in GDLisp.

``Notification``
----------------

::

   (defenum Notification
     (POSTINITIALIZE ...)
     (PREDELETE ...))

An enumeration representing the Godot notification constants `defined
on Object
<https://docs.godotengine.org/en/stable/classes/class_object.html#constants>`_.

``Number``
----------

::

   (defclass Number (AnyVal)
     ...)

The type of numbers in GDLisp. Integers and floating-point numbers are
both instances of this type.
