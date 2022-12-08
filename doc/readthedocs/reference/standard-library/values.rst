
Global Values
=============

Constant values defined in the GDLisp value namespace at global scope
are documented here.

``GODOT-VERSION``
-----------------

This constant is defined to be an integer value representing the Godot
version that ``GDLisp.lisp`` was compiled with, as follows

.. code-block:: text

   GODOT-VERSION = 1,000,000 * major-version + 10,000 * minor-version + 100 * patch-version

So, for instance, Godot 3.5.0 

``nil``
-------

The value ``nil`` is defined to be the special null object ``()``.

