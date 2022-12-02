
Structure of a GDLisp Source File
=================================

A GDLisp source file is, by convention, written with a ``.lisp`` file
extension. GDLisp source files will be compiled into GDScript source
files (``.gd``) in a one-to-one fashion, with the resulting compiled
GDScript file taking the same filename as the input file, merely with
the extension changed. So, for example, ``Player.lisp`` would be
compiled to ``Player.gd``. GDLisp source files are *always* encoded in
UTF-8.

A GDLisp source file defines a module. Syntactically, a GDLisp source
file consists of zero or more S-expressions. With the exception of
quoted and quasiquoted terms, all lists appearing in GDLisp source
code must be *proper lists*.

Order of Definition
-------------------

Generally speaking, classes and functions defined in a GDLisp module
can reference each other freely and can be defined in any order. /////
