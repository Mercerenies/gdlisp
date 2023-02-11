
.. _imports:

Import Directives
=================

GDLisp uses the special ``use`` directive to import names and
resources from other files. Although it is technically possible to
import resources in the GDScript way (by defining constants whose
values are ``preload`` calls), it is not recommended.

There are three different forms to the ``use`` directive, all of which
begin with the symbol ``use``, followed by the path name as a literal
string.

::

  ;; Qualified import
  (use "res://filename.lisp" as alias-name)

  ;; Open import
  (use "res://filename.lisp" open)

  ;; Explicit import
  (use "res://filename.lisp" (...))

The path name is always a string and follows the same rules as Godot
paths. See `File system
<https://docs.godotengine.org/en/stable/tutorials/scripting/filesystem.html>`_
for details.

Importing names involves adding names (in either of the value or
function namespaces) to the module's lexical scope. Names added by a
``use`` directive are always imported as ``private``, and hence cannot
be transitively imported into other modules through the current one.

Qualified Imports
-----------------

::

  (use "res://filename.lisp")
  (use "res://filename.lisp" as alias-name)

A qualified import takes the form of either the ``use`` symbol
followed by the path name and nothing else, or the ``use`` symbol
followed by ``as`` and then followed by the desired alias name. If the
alias name is not provided, then the default alias shall be the name
of the file, without the path or the file extension. So the import
``"res://foo/bar/baz.lisp`` would have a default alias name of
``foo/bar/baz``. Remember that forward slashes are valid in
identifiers in GDLisp.

Every public identifier in the target module will be imported into the
current scope, in the appropriate namespace. The name in the current
scope shall be the import's alias name, followed by a forward slash,
followed by the identifier's name in the source module. As an example,
suppose the module ``MyModule.lisp`` defines a public function called
``get-name``. Then the directive ``(use "res://MyModule.lisp" as
MySpecialModule)`` would import that function into the current scope
as ``MySpecialModule/get-name``.

Non-GDLisp Imports
^^^^^^^^^^^^^^^^^^

The ``use`` directive is used for both importing other GDLisp files,
as well as for importing GDScript files, scenes, textures, and
essentially anything you would use ``preload`` for in GDScript.
However, the GDLisp compiler can only parse other GDLisp files, so any
non-GDLisp file must be imported using the qualified import, not an
explicit or open import. This restriction may be loosened in the
future.

In the case of a non-GDLisp file, the alias name itself is introduced
into the module's lexical scope in the value namespace. In the case of
a GDScript file, the name refers to the top-level class defined in
that file. In the case of other resources, the name refers to the
resource itself.

Open Imports
------------

::

   (use "res://filename.lisp" open)

An open import is similar to a qualified import except without the
qualifier. An open import introduces all of the public names from the
target module into the current scope, keeping their names identical to
the original names from the source module.

Explicit Imports
----------------

::

  (use "res://filename.lisp" (...))

An explicit import does *not* import all of the public names into the
current scope. Instead, it imports only the names specified. Each
element of the list that follows the pathname identifies a single name
to be imported. The most general form for these elements is

::

   (source-name namespace as destination-name)

where ``source-name`` and ``destination-name`` are arbitrary
identifiers, and ``namespace`` is one of ``value`` or ``function``.
This imports the name ``source-name`` from the given namespace into
the current module scope (in the same namespace), giving it the name
``destination-name``.

The ``as destination-name`` portion may be omitted and, if left out,
will be assumed to be the same as the ``source-name``. The
``namespace`` may also be omitted if unambiguous. If the namespace is
omitted, then it will be inferred from the available names in the
source module. If there is *both* a function and a value with that
name in the source module, then an error will be issued. Finally, if
both the alias name and the namespace are omitted, then the symbol for
the name must be passed on its own, not inside of a sublist. In
summary, the four possible forms of a named import are

::

   (source-name namespace as destination-name)
   (source-name namespace) ;; destination-name implied to be source-name
   (source-name as destination-name) ;; namespace is inferred from context
   source-name ;; namespace is inferred, and destination-name is implied to be source-name
