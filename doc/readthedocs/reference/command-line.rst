
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

Once the dependencies are installed, to build the release version of
GDLisp, it should suffice to run

.. code-block:: bash

   $ GDLISP_RELEASE=1 rake build

.. Note:: If you're building in a non-POSIX shell, you may have to
          initialize the environment variable ``GDLISP_RELEASE`` in
          some other way.

After a successful build for release purposes, an executable symlink ////

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
