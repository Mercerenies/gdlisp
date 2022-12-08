
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

``append``
----------

::

   (defn append (&rest args)
     ...)

Appends all of the lists together, returning a list which contains all
of the elements of each argument, in the same order they appeared.

The resulting list will share structure with the final argument to
``append``. That is, the cons cells leading up to the final argument
to ``append`` will *not* be rebuilt. Additionally, the final argument
to ``append`` can be an improper list, while all of the other
arguments must be proper lists.

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

``list/elt``
------------

::

   (defn list/elt (list n)
     ...)

Given a list and an index, returns the element of the list at that
0-indexed position. Produces an error if the index is out of bounds.

``list/filter``
---------------

::

   (defn list/filter (p xs)
     ...)

Applies the unary predicate ``p`` to each element of ``xs`` and
returns a list of all elements for which the predicate returned
truthy. The returned list shares no structure with the argument list.

``list/fold``
-------------

::

   (defn list/fold (f xs &opt x)
     ...)

A left-fold over a list. ``f`` shall be a function of two arguments,
``xs`` shall be a proper list, and ``x`` (if supplied) shall be a
non-null starting value.

The list is traversed from the beginning to the end. At each list
element, the call ``(funcall f acc element)`` is made, where ``acc``
is the value we've accumulated so far and ``element`` is the current
element. The return value of that function call is used as the new
value of ``acc``. At the end, ``acc`` is returned.

The initial value of ``acc`` is ``x`` if supplied. If ``x`` is not
supplied, then the initial value is the first element of the list, and
iteration begins at the second. If ``x`` is not supplied and the list
is empty, then an error is produced.

``list/map``
------------

::

   (defn list/map (f xs)
     ...)

Applies the unary function ``f`` to each element of the list ``xs``
and returns a new list of the result values.

``list/reverse``
----------------

::

   (defn list/reverse (arg)
     ...)

Returns a list containing all of the same elements as ``arg`` but in
reverse order. Does not mutate ``arg``.

``list/tail``
-------------

::

   (defn list/tail (list k)
     ...)

Returns the ``k``\ th cdr of ``list``. That is, returns a tail of the
list with the first ``k`` elements removed. The resulting list shares
structure with ``list``.

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
