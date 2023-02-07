
Built-in Functions
==================

All of the GDLisp global functions are documented here. These
functions are available (in the function namespace) at the top-level
of every GDLisp file.

Note that most of the `GDScript built-in functions
<https://docs.godotengine.org/en/stable/classes/class_%40gdscript.html>`_
are available unmodified in GDLisp as well. All names in GDScript
which contain an underscore have their underscores replaced with
dashes in GDLisp, so ``get_stack`` translates to ``get-stack`` in
GDLisp for instance. The exceptions to this are:

* ``assert``, ``preload``, and ``yield`` are not functions and are
  instead special forms in GDLisp. See :ref:`expr-assert`,
  :ref:`expr-preload`, and :ref:`expr-yield`, respectively.

* ``typeof`` in GDLisp behaves differently. It is documented at
  :ref:`function-typeof`.

* ``convert`` has been wrapped to support GDLisp synthetic types. See
  :ref:`function-convert`.

* ``len`` has been wrapped in GDLisp to support GDLisp lists in
  addition to strings, arrays, and dictionaries. See
  :ref:`function-len`.

``*``
-----

::

   (defn * (&rest args)
     ...)

Multiplies all of the arguments (which must be numerical or vectors)
together. If given no arguments, returns the integer 1.

``+``
-----

::

   (defn + (&rest args)
     ...)

Adds all of the arguments (which must be numerical or vectors)
together. If given no arguments, returns the integer 0.

To concatenate strings in GDLisp, use the built-in Godot function
``str``, which accepts multiple strings and concatenates them
together.

``-``
-----

::

   (defn - (x &rest args)
     ...)

Subtracts all of the arguments from the first in order. If given only
one argument, returns its negation. That is, ``(- x)`` is equivalent
to ``(- 0 x)``.

``/``
-----

::

   (defn / (x &rest args)
     ...)

Divides all of the arguments out of the first in order. If given only
one argument, returns its reciprocal. That is, ``(/ x)`` is equivalent
to ``(/ 0 x)``.

``/=``
------

::

   (defn /= (x &rest args)
     ...)

Returns true if *none* of the arguments are equal to each other. Note
that since ``/=`` is non-transitive, this is different than just
checking whether *adjacent* arguments are equal. That is, ``(/= 1 2
1)`` is *false* even though no adjacent elements are equal.

``<``
-----

::

   (defn < (x &rest args)
     ...)

Returns true if each argument (in order) is less than the following
element, according to their natural ordering.

``<=``
------

::

   (defn <= (x &rest args)
     ...)

Returns true if each argument (in order) is less than or equal to the
following element, according to their natural ordering.

``=``
-----

::

   (defn = (x &rest args)
     ...)

Checks whether each argument is equal to the argument following it.

``>``
-----

::

   (defn > (x &rest args)
     ...)

Returns true if each argument (in order) is greater than the following
element, according to their natural ordering.

``>=``
------

::

   (defn >= (x &rest args)
     ...)

Returns true if each argument (in order) is greater than or equal to
the following element, according to their natural ordering.

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

.. _function-apply:

``apply``
-----------

::

   (defn apply (f &rest args)
     ...)

Calls the function object ``f`` with the given arguments, returning
the result of the function call. At least one argument must be
supplied, and the final argument is treated as a list of arguments,
not an individual one.

For a version of ``apply`` without the "variable argument" behavior,
see :ref:`function-funcall`.

The difference between this function and ``funcall`` is that the
former treats its final argument as a list and prepends the others
onto it. Concretely, all of the following will sum the numbers from
one to four, returning ten::

  (+ 1 2 3 4)
  (funcall #'+ 1 2 3 4)
  (apply #'+ 1 2 3 4 ())
  (apply #'+ '(1 2 3 4))
  (apply #'+ 1 2 '(3 4))

``array``
----------

::

   (defn array (&rest xs)
     ...)

Constructs a array of the arguments given. The ``[ ... ]`` syntax
desugars to a call to this function.

``array->list``
---------------

::

   (defn array->list (arr)
     ...)

Converts a Godot array or pooled array into a proper list.

.. _function-array-filter:

``array/filter``
----------------

::

   (defn array/filter (p xs)
     ...)

Applies the unary predicate ``p`` to each element of the array ``xs``
and returns a filter of all elements for which the predicate returned
truthy. Does not modify ``xs``.

.. _function-array-find:

``array/find``
----------------

::

   (defn array/find (p arr &opt default)
     ...)

Applies the unary predicate ``p`` to each element of the array
``arr``. Returns the first element of the array for which the
predicate returned true. If no element returns true, then ``default``
is returned. This function will short-circuit and stop calling ``p``
as soon as a match is found.

.. _function-array-fold:

``array/fold``
--------------

::

   (defn array/fold (f xs &opt x)
     ...)

A left-fold over an array. ``f`` shall be a function of two arguments,
``xs`` shall be a Godot array, and ``x`` (if supplied) shall be a
non-null starting value.

The array is traversed from the beginning to the end. At each element,
the call ``(funcall f acc element)`` is made, where ``acc`` is the
value we've accumulated so far and ``element`` is the current element.
The return value of that function call is used as the new value of
``acc``. At the end, ``acc`` is returned.

The initial value of ``acc`` is ``x`` if supplied. If ``x`` is not
supplied, then the initial value is the first element of the array,
and iteration begins at the second. If ``x`` is not supplied and the
array is empty, then an error is produced.

.. _function-array-map:

``array/map``
-------------

::

   (defn array/map (f xs)
     ...)

Applies the unary function ``f`` to each element of the array ``xs``
and returns a new array of the returned values. Does not modify
``xs``.

``array/reverse``
-----------------

::

   (defn array/reverse (arg)
     ...)

Returns a new array containing all of the same elements as the input
array ``arg`` but in reverse order. Does not mutate ``arg``.

``cons``
--------

::

   (defn cons (a b)
     ...)

Constructs a fresh cons cell, with the first argument as car and the
second as cdr.

.. _function-convert:

``convert``
-----------

::

   (defn convert (what type)
     ...)

Converts the value ``what`` into the type ``type``, according to the
Godot rules for the GDScript function of the same name. ``type`` can
be a primitive type object (such as ``Int``) or a type enumeration
constant (such as ``Type:INT``).

``dict``
----------

::

   (defn dict (&rest xs)
     ...)

Constructs a dictionary, where each even-indexed argument is a key
paired with the value immediately after it. of the arguments given.
The ``{ ... }`` syntax desugars to a call to this function. The
behavior is undefined if this function is called with an odd number of
arguments.

``dict/elt``
------------

::

   (defn dict/elt (dict k)
     ...)

Gets the value corresponding to key ``k`` of the dictionary ``dict``.

``equal?``
----------

::

   (defn equal? (x &rest args)

Returns whether each value is equal to the one after it, following
data structures recursively. This function is similar to the built-in
Godot function ``deep_equal``, but ``equal?`` works on more data
types. Specifically, ``equal?`` will recursively follow arrays (of any
type), dictionaries, and GDLisp lists.

``elt``
-------

::

   (defn elt (x y)
     ...)

Gets the element from the data structure. ``(elt x y)`` is guaranteed
to compile to the Godot ``x[y]``, hence can be used on arrays,
dictionaries, and any other data structure that the subscript operator
is compatible with.

.. _function-funcall:

``funcall``
-----------

::

   (defn funcall (f &rest args)
     ...)

Calls the function object ``f`` with the given arguments, returning
the result of the function call. Note that, since function objects do
not know their own function signature at compile-time, the number of
arguments cannot be validated at compile time when using ``funcall``.
An error will be issued at runtime, if the argument count does not
match up.

Example::

  (let ((my-function (lambda (x y) (+ x y))))
    (funcall my-function 10 15)) ; Returns 25

For a version of ``funcall`` that takes a list of arguments rather
than individual arguments, see :ref:`function-apply`.

``gcd``
-------

::

   (defn gcd (&rest args)
     ...)

Returns the greatest common divisor of the arguments, or 0 if no
arguments are given.

``gensym``
----------

::

   (defn gensym (&opt prefix)
     ...)

Returns a new symbol object whose name has not appeared in the GDLisp
source code that the compiler has encountered up to this point and
that has not been interned dynamically with ``intern``.

If ``prefix`` is provided, then it must be a string and the returned
symbol will have a name that begins with ``prefix``. If not, then a
default prefix will be chosen for you.

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

``instance?``
-------------

::

   (defn instance? (value type)
     ...)

Given a value and a type object, returns whether or not that value is
an instance of that type. The type object can be any of the following.

* A GDScript or GDLisp class.
* The name of a built-in Godot object type, such as ``Node`` or
  ``Spatial``.
* A Godot primitive type, such as ``Int``.
* A GDLisp abstract type, such as ``BaseArray`` or ``AnyVal``.

``intern``
----------

::

   (defn intern (s)
     ...)

Returns a symbol object whose name is ``s``. If the given symbol
already exists in the global symbol table, then the *same* object (up
to ``=`` comparison) is returned. Otherwise, a new symbol object is
created and returned.

``last``
--------

::

   (defn last (a)
     ...)

Returns the final element of ``a``, which must be a nonempty list
(proper or improper). The terminator of an improper list is ignored
(it is not considered the final element, even if it is nontrivial).

``lcm``
-------

::

   (defn lcm (&rest args)
     ...)

Returns the least common multiple of the arguments, or 1 if no
arguments are given.

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

``list/copy``
-------------

::

   (defn list/copy (list)
     ...)

Returns a new list containing the same elements as ``list`` in the
same order. The two lists will not share any structure.

``list/filter``
---------------

::

   (defn list/filter (p xs)
     ...)

Applies the unary predicate ``p`` to each element of ``xs`` and
returns a list of all elements for which the predicate returned
truthy. The returned list shares no structure with the argument list.

``list/find``
----------------

::

   (defn list/find (p xs &opt default)
     ...)

Applies the unary predicate ``p`` to each element of the proper list
``xs``. Returns the first element of the list for which the predicate
returned true. If no element returns true, then ``default`` is
returned. This function will short-circuit and stop calling ``p`` as
soon as a match is found.

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

``max``
-------

::

   (defn max (&rest args)
     ...)

Returns the maximum of all of the arguments given, or ``- INF`` if
given no arguments.

``member``
----------

::

   (defn member? (value structure)
     ...)

Returns whether or not the value is a member of the structure. This is
guaranteed to compile to the Godot ``in`` infix operator and has the
same semantics.

``min``
-------

::

   (defn min (&rest args)
     ...)

Returns the minimum of all of the arguments given, or ``INF`` if given
no arguments.

``mod``
-------

::

   (defn mod (x y)
     ...)

Returns the (integer) modulo of ``x`` by ``y``. As in Godot, to take a
floating-point modulo, use the built-in GDScript function ``fmod``.

``NodePath``
------------

::

   (defn NodePath (string)
     ...)

Constructs a ``NodePath`` object containing the given string.

``not``
-------

::

   (defn not (x)
     ...)

Returns a proper Boolean (i.e. either ``#t`` or ``#f``) of the
*opposite* truthiness to ``x``. So if ``x`` is truthy then this
function returns ``#f``, and if ``x`` is falsy then this function
returns ``#t``.

``set-dict/elt``
----------------

::

   (defn set-dict/elt (x dict k)
     ...)

Sets the value corresponding to key ``k`` of the dictionary ``dict``
to ``x``. If the key does not exist in the dictionary, then it is
inserted.

``set-elt``
-----------

::

   (defn set-elt (x y z)
     ...)

Sets the element from the data structure. ``(set-elt x y z)`` is guaranteed
to compile to the Godot ``y[z] = x``, hence can be used on arrays,
dictionaries, and any other data structure that the subscript operator
is compatible with.

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

``vector``
----------

::

   (defn vector (x y &opt z)
     ...)

Constructs a vector of two or three dimensions with the given
arguments. The ``V{ ... }`` syntax desugars to calls to this function.
