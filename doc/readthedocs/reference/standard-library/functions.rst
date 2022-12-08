
Built-in Functions
==================

All of the GDLisp global functions are documented here. These
functions are available (in the function namespace) at the top-level
of every GDLisp file.

Note that most of the `GDScript built-in functions
<https://docs.godotengine.org/en/stable/classes/class_%40gdscript.html>`_
are available unmodified in GDLisp as well. The exceptions to this are:

* ``assert``, ``preload``, and ``yield`` are not functions and are
  instead special forms in GDLisp. See :ref:`expr-assert`,
  :ref:`expr-preload`, and :ref:`expr-yield`, respectively.

* ``typeof`` in GDLisp behaves differently. It is documented at
  :ref:`function-typeof`.

.. _function-typeof:

``typeof``
----------

::

   (defn typeof (value)
     ...)

Given a value, return a type object representing its type. Note that
this is **not** the same as the GDScript ``typeof`` function, which
returns an integer constant. The object returned by the GDLisp
``typeof`` function is an object representing the value's most
specific type, which can be passed to ``instance?`` for instance
checks.

* In the case of a primitive value (i.e. an instance of a subtype of
  ``AnyVal``), a special type object is returned. This type object can
  only be passed to ``instance?`` and does not have any public fields
  or methods.

* In the case of an object whose type is defined in GDLisp or
  GDScript, the script resource or inner class resource is returned.

* In the case of a direct instance of a Godot native type like
  ``Node``, the relevant native type object is returned.
