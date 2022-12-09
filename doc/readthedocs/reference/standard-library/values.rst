
Global Values
=============

Constant values defined in the GDLisp value namespace at global scope
are documented here.

Note that all constants defined in the global scope for use in
GDScript are also available (unmodified) in GDLisp. That includes all
of the following:

* `@GDScript Constants <https://docs.godotengine.org/en/stable/classes/class_%40gdscript.html#constants>`_

* `@GlobalScope Constants <https://docs.godotengine.org/en/stable/classes/class_%40globalscope.html#constants>`_

* `@GlobalScope Enumerations <https://docs.godotengine.org/en/stable/classes/class_%40globalscope.html#enumerations>`_

Additionally, the constants defined on the root ``Object`` type are
all exposed globally in GDLisp. These include: ``CONNECT_DEFERRED``,
``CONNECT_PERSIST``, ``CONNECT_ONESHOT``,
``CONNECT_REFERENCE_COUNTED``, ``NOTIFICATION_POSTINITIALIZE``, and
``NOTIFICATION_PREDELETE``.

``GDLisp``
----------

::

   (defconst GDLisp ...)

The top-level constant called ``GDLisp`` refers to the GDLisp node
itself. That is, the top-level name ``GDLisp`` can be used to
reference the singleton object that represents the ``GDLisp.lisp``
support library. It is generally not necessary to refer to this object
directly, since GDLisp does the name resolution for you, but the name
is available if needed.

``GODOT-VERSION``
-----------------

::

   (defconst GODOT-VERSION ...)

This constant is defined to be an integer value representing the Godot
version that ``GDLisp.lisp`` was compiled with, as follows

.. code-block:: text

   GODOT-VERSION = 1,000,000 * major-version + 10,000 * minor-version + 100 * patch-version

So, for instance, Godot 3.4.1 would be represented by the
``GODOT-VERSION`` constant ``3040100``. These version integers are
constructed in such a way that two version integers can be compared
using the standard (numerical) comparison operators, to check if the
version of Godot is newer or older than some set value.

Note that this checks the version of Godot that ``GDLisp.lisp`` was
compiled with, not the one being used at runtime. To get the version
of Godot that is currently *running*, use the built-in Godot function
`get_version_info
<https://docs.godotengine.org/en/stable/classes/class_engine.html#class-engine-method-get-version-info>`_.

``nil``
-------

::

   (defconst nil ())

The value ``nil`` is defined to be the special null object ``()``.

