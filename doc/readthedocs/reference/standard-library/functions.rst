
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

* ``len`` has been wrapped in GDLisp to support GDLisp lists in
  addition to strings, arrays, and dictionaries. See
  :ref:`function-len`.

``array->list``
---------------

::

   (defn array->list (arr)
     ...)

Converts a Godot array or pooled array into a proper list.

``cons``
--------

::

   (defn cons (a b)
     ...)

Constructs a fresh cons cell, with the first argument as car and the
second as cdr.

``init``
--------

::

   (defn init (a)
     ...)

Returns a new list which consists of all of the elements of the
original list ``a`` *except* the final one. ``a`` can be a proper or
improper list, and in either case the terminator is ignored (it is not
considered the final element, even if it is nontrivial), and the
resulting list shall be proper.

``a`` must be nonempty.

``last``
--------

::

   (defn last (a)
     ...)

Returns the final element of ``a``, which must be a nonempty list
(proper or improper). The terminator of an improper list is ignored
(it is not considered the final element, even if it is nontrivial).

.. _function-len:

``len``
-------

::

   (defn len (x)
     ...)

Returns the length of the list, array, dictionary, or string object.

``list``
--------

::

   (defn list (&rest args)
     ...)

Returns a proper list containing all of the arguments, in the same
order.

``list->array``
---------------

::

   (defn list->array (list)
     ...)

Converts a proper list into a Godot array.

``snoc``
--------

::

   (defn snoc (a b)
     ...)

Appends an element ``b`` to the end of a proper list ``a``. The list
``a`` is not modified, and the returned list is a newly-constructed
one.

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
