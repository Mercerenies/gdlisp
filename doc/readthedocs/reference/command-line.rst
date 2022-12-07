
The GDLisp Command Line Tool
============================

The core interface to the GDLisp compiler and programming language is
the command line tool ``gdlisp``. This tool is written mostly in Rust,
with a small portion written in GDScript for bootstrapping purposes.

Building
--------

``gdlisp`` is built using Rake, the Ruby task management tool. The
following dependencies must be explicitly installed before building
``gdlisp``.

* The ``rake`` command line tool must be installed, as it is necessary
  to run rakefiles.

* The target version of Godot must be available on the target system
  and must be available on the system path under the name ``godot``
  (case sensitive).

* If building on Windows, you will also need the `win32-file gem
  <https://www.rubydoc.info/gems/win32-file>`_.

Once the dependencies are installed, to build the release version of
GDLisp, it should suffice to run

.. code-block:: bash

   $ GDLISP_RELEASE=1 rake build

.. Note:: If you're building in a non-POSIX shell, you may have to
          initialize the environment variable ``GDLISP_RELEASE`` in
          some other way.

After a successful build for release purposes, an executable will be
placed at ``./bin/gdlisp``. At this point, if you plan to use GDLisp
long-term, then you may wish to place that file on your system path.

The Rakefile
^^^^^^^^^^^^

The Rakefile for this tool provides the following tasks.

``rake build``
    Builds the entire project, including all dependencies.

``rake clean``
    Cleans up any build artifacts.

``rake run``
    Builds and then runs the ``gdlisp`` program.

``rake build_rs``
    Builds only the Rust dependencies, not the Godot dependencies.

``rake test``
    Runs the full GDLisp test suite.

``rake clippy``
    Runs the Rust linter Clippy against the codebase.

``rake doc``
    Generates the internal documentation of the codebase. That is
    *not* the page you're reading right now but consists of Rustdoc
    pages detailing the inner workings of the source code.

When building the software, the build suite defaults to compiling in
*debug mode*, which disables certain optimizations and produces better
internal stack traces in GDLisp. To make a build for release purposes,
define the ``GDLISP_RELEASE`` environment variable to any value.

Running
-------

Once you have the ``gdlisp`` command line tool, you may run it with
``--help`` to see the available options. Generally speaking, users of
GDLisp will invoke the program in the following ways.

* Invoking ``gdlisp`` with no arguments will open a REPL instance.

* Invoking ``gdlisp`` with the name of one or more ``.lisp`` filenames
  will compile all of those files from GDLisp into GDScript.

* Invoking ``gdlisp`` with the name of a directory will recursively
  search that directory, compiling all ``.lisp`` files into GDScript
  files. This is most useful if given the root directory of a Godot
  project (i.e. the folder containing your ``project.godot`` file).

The REPL
--------

Invoking ``gdlisp`` with no arguments drops you into a read-eval-print
loop (or REPL, for short), where you can run arbitrary GDLisp
declarations or expressions and see the output.

.. _support-file:

The GDLisp Support File
-----------------------

GDLisp has a single-file support library that **must** be included in
any project that uses GDLisp source code. This file defines all of the
built-in functions and values available to GDLisp programmers, as well
as providing some scaffolding necessary for the generated code as
well.

In a project that needs to utilize GDLisp source code, you must
include the ``GDLisp.gd`` file. This file is bootstrapped from
``GDLisp.lisp`` and is produced into the root project directory as
part of the build process above.

1. Copy and paste the ``GDLisp.gd`` file into the root of your project
   directory.

2. Create a Godot autoload (sometimes called a singleton) for this
   file in your project. The autoload must be called ``GDLisp`` (with
   that exact capitalization). All GDLisp source files will assume
   that this global variable exists.

.. Warning:: Some versions of Godot try to choose the capitalization
             for your autoloads in a different way. Specifically,
             Godot may suggest the name ``GdLisp`` (with a lowercase
             ``d``) as a default name. The name of the autoload must
             be ``GDLisp``, with that *exact* capitalization, or
             GDLisp generated source files will not load.
