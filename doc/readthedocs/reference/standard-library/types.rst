
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

``Notification``
----------------

::

   (defenum Notification
     (POSTINITIALIZE ...)
     (PREDELETE ...))

An enumeration representing the Godot notification constants `defined
on Object
<https://docs.godotengine.org/en/stable/classes/class_object.html#constants>`_.

