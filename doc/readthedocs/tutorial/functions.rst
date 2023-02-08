
Functions and Modules
=====================

Now that we have some basic tools to do calculations and store
variables in GDLisp, it's time to talk about how to write independent
GDLisp source files.

Modules
-------

This is one of the key places that GDLisp differs from GDScript, and
it's important to understand the distinction. In GDScript, every
source file you write represents a GDScript resource, effectively a
class in Python terminology. A GDLisp source file does *not*
automatically produce an instantiable class. There are ways to declare
a class, and we will discuss those in the next section. But a GDLisp
source file has a one-to-one correspondence with a *module*. A module
is a collection of named classes, functions, and constants that can be
imported and used in other modules.

Functions
---------

We've already seen how to declare local functions inside of a block of
code, either as a reified value with ``lambda`` or as a scoped local
name with ``flet`` or ``labels``. At the top-level of a module, named
functions are declared with the ``defn`` declaration form.

::

   (defn function-name (args ...)
     body ...)

GDLisp functions are, by default, public to the module, which means
other modules are free to import and use the function. Private
module-level functions can be declared with the ``private`` modifier::

   (defn function-name (args ...) private
     body ...)

A function declared in this way can be used freely inside the current
module but cannot be imported or used in another module.

To call a function, simply use its name as the first symbol of an
S-expression::

  (function-name arg1 arg2 arg3 ...)

The formal argument list of a function supports optional and variadic
arguments. To declare a function with optional arguments, use the
``&opt`` directive::

   (defn compare-strings (a b &opt case-sensitive)
     (if case-sensitive
       (a:casecmp_to b)
       (a:nocasecmp_to b)))

This declares a function ``compare-strings`` that accepts two or three
arguments. If the third argument ``case-sensitive`` is not supplied,
it defaults to ``()`` (which is falsy).

Variadic arguments can be declared with ``&rest`` or ``&arr``. The
former groups all extra arguments into a GDLisp list, and the latter
groups all extra arguments into a Godot array.

::

   (defn make-array (&arr args)
     args)

   (make-array 1 2 3 4 5) ; Produces the array [1 2 3 4 5]

GDLisp standalone function calls always have their argument count
validated at compile-time. If a function is called with the wrong
number of arguments, the GDLisp compiler will emit an error.

Constants
---------

Constants are declared with ``defconst``.
