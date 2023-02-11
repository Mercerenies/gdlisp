
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

Note that the same argument directives ``&opt``, ``&rest``, and
``&arr`` can be applied to local functions declared with ``flet``,
``labels``, or ``lambda``. The only difference is that in the case of
``lambda``, the function is reified into an object, so the argument
validation happens at runtime rather than compile-time.

Constants
---------

Constants are declared with ``defconst``. Like functions, constants
are public by default but can be made private to the module.

Examples::

  (defconst DAYS_IN_YEAR 365)
  (defconst DEFAULT_PLAYER_NAME "Steve")
  (defconst EPOCH 1970 private) ; Private constant

.. Important:: Unlike GDScript, GDLisp does **not** use constants to
               load resources and other scripts. GDLisp has a
               dedicated import syntax for loading other files,
               whether those files are resources, source files, or
               packed scenes.

Enumerations
------------

Enumerations are declared with ``defenum``.

::

  (defenum Color RED YELLOW BLUE)

This defines an enumeration constant called ``Color`` with three
elements: ``Color:RED``, ``Color:GREEN``, and ``Color:BLUE``. Note
that we refer to the elements of an enumeration with a colon ``:``, as
opposed to a dot ``.`` like we would in GDScript. We'll see this
syntax again when we discuss classes.

Enum elements can optionally be given specific numerical values. For
example::

  (defenum NUMBERS
    (ONE 1)
    (TEN 10)
    (ONE_HUNDRED 100))

As with constants and functions, an enumeration can be made private to
the enclosing module.

::

  (defenum NUMBERS private
    (ONE 1)
    (TEN 10)
    (ONE_HUNDRED 100))

Importing Modules
-----------------

Modules are of little use if they can't be imported and reused in
other modules. In GDScript, we load other resources, including other
GDScript source files, with the ``preload`` function, as follows.

.. code-block:: gdscript

   const MyScript = preload("res://MyScript.gd")
   const MySprite = preload("res://MySprite.png")
   const MyScene = preload("res://MyScene.tscn")

This is still technically possible to do in GDLisp, but it's not
idiomatic, and it complicates macro expansion. GDLisp has a special,
and very versatile, ``use`` directive that's designed for importing
data from other files.

Importing Resources
^^^^^^^^^^^^^^^^^^^

Importing non-GDLisp resources is simple.

::

   (use "res://MySprite.png" as MySprite)
   (use "res://MyScene.png" as MyScene)

After the keyword ``use``, we write the path of the resource, using
the same ``res://`` syntax as a GDScript ``preload``. Then we specify
how we'd like to name the resource in the current scope. The above
snippet defines two constants in the current scope: ``MySprite`` and
``MyScene``.

Note that names imported into a module are *not* transitively
re-exported, so while our hypothetical module above has access to the
names ``MySprite`` and ``MyScene``, other modules that *import* our
module cannot import those names from it.

If you don't specify an alias for the import, one will be chosen for
you based on the name of the resource. The following two lines are
equivalent::

   (use "res://Example/MySprite.png" as MySprite)
   (use "res://Example/MySprite.png")

This is also how source files written in GDScript are imported into
GDLisp. The entire GDScript resource is imported as a single name
which the GDLisp module can access.

Importing Other GDLisp Modules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Importing GDLisp modules works a bit differently. A GDLisp module is a
collection of functions and other names, not just a single runtime
resource. If we use the same import syntax as above::

   (use "res://MyModule.lisp" as MyModule)

Then every public name from the module ``MyModule`` will be imported
into the current module, with the prefix ``MyModule`` added.
Concretely, suppose ``MyModule.lisp`` contained these names::

   (defn a-function () ...)

   (defn another-function () ...)

   (defconst MY_FIRST_CONSTANT 1)
   (defconst MY_SECOND_CONSTANT 2 private)

Then the ``use`` directive given above would have the following
effects:

* The function ``a-function`` is imported into the current scope,
  under the name ``MyModule/a-function``.

* The function ``another-function`` is imported into the current
  scope, under the name ``MyModule/another-function``.

* The constant ``MY_FIRST_CONSTANT`` is imported into the current
  scope, under the name ``MyModule/MY_FIRST_CONSTANT``.

* The constant ``MY_SECOND_CONSTANT`` is *not* imported, as the name
  is private and not visible to external modules.

As with non-GDLisp resources, if an alias is not specified, then one
will be chosen for you based on the full path of the module.

Explicit Imports
""""""""""""""""

Instead of providing a prefix to insert before all names from a
module, you may instead specify explicitly which names you want to
import (without a prefix) from the module.

::

   (use "res://MyModule.lisp" (a-function MY_FIRST_CONSTANT))

In this example, the function ``a-function`` and the constant
``MY_FIRST_CONSTANT`` will be imported as-is into the current scope
(with no prefix), and ``another-function`` will not be imported at
all.

Aliases can be provided, for any explicit imports.

::

   (use "res://MyModule.lisp" ((a-function as an-external-function)
                               (MY_FIRST_CONSTANT as CONSTANT)))

Finally, if you want to import all of the (public) names from a
module, without any prefix, you may replace the explicit import list
with the word ``open``.

::

   (use "res://MyModule.lisp" open)

GDLisp.lisp
-----------

If you're compiling your own GDLisp modules, there's one more
dependency you need to know about. All of the functions and constants
that are defined in GDLisp are provided by a support library, called
``GDLisp.lisp``. This support library is included in the GDLisp
package you downloaded and is automatically compiled into
``GDLisp.gd`` when you build the GDLisp compiler. ``GDLisp.gd`` must
be included as an autoloaded singleton (with the name ``GDLisp``) in
any project that uses GDLisp code.

To include this in your project, simply copy the ``GDLisp.gd`` file
into your project's root directory and add it in the autoloads list
under your project settings, ensuring that the name of the autoload is
``GDLisp`` (this is case-sensitive). **All** GDLisp modules implicitly
assume that this autoload is available, so you **must** include it in
any project that includes GDLisp code.
