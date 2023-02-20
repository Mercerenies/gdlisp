[![GitHub Actions: Ubuntu](https://github.com/mercerenies/gdlisp/actions/workflows/test-Godotv3.5-Ubuntu.yml/badge.svg)](https://github.com/Mercerenies/gdlisp/actions/workflows/test-Godotv3.5-Ubuntu.yml)
[![GitHub Actions: Ubuntu](https://github.com/mercerenies/gdlisp/actions/workflows/test-Godotv3.5-Mono-Ubuntu.yml/badge.svg)](https://github.com/Mercerenies/gdlisp/actions/workflows/test-Godotv3.5-Mono-Ubuntu.yml)
[![GitHub Actions: Ubuntu](https://github.com/mercerenies/gdlisp/actions/workflows/test-Godotv3.4-Ubuntu.yml/badge.svg)](https://github.com/Mercerenies/gdlisp/actions/workflows/test-Godotv3.4-Ubuntu.yml)
[![GitHub Actions: Ubuntu](https://github.com/mercerenies/gdlisp/actions/workflows/test-Godotv3.3.3-Ubuntu.yml/badge.svg)](https://github.com/Mercerenies/gdlisp/actions/workflows/test-Godotv3.3.3-Ubuntu.yml)

# GDLisp

Lisp for the [Godot](https://godotengine.org/) platform! This language
aims to be a Lisp dialect which compiles to GDScript.

This project is built using [Cargo](https://doc.rust-lang.org/cargo/)
with [Rake](https://ruby.github.io/rake/) as a wrapper for custom
build scripts. Use `rake test` to run the test suite, and use `rake
run` to compile stdin input to GDScript.

The current version of GDLisp is tested against Godot 3.5 and expects
a command called `godot` to be on your system's path which points to
the Godot executable.

## Features

* Support for standard GDScript functionality, including function
  declarations, class declarations, signals, etc.
* The expression-based semantics we all love from Lisp
* Lambdas / anonymous functions as well as anonymous classes
* Support for compile-time macros that run in the GDLisp compiler
  itself

## FAQs

### Can I use GDLisp today?

Absolutely! GDLisp is production-ready, and I encourage everyone to
try it out and provide any feedback on the issue tracker.

### What Godot versions is GDLisp compatible with?

GDLisp works with Godot 3.x. The next major release of GDLisp will be
fully compatible with Godot 4, though there's no definite timeline on
that as yet.

### Can I use GDLisp and GDScript in the same project?

You certainly can! GDLisp constructs compile in a straightforward way
to existing Godot concepts. Classes and functions written in GDLisp
can be used from GDScript, and vice versa.

## License

GDLisp is distributed under the terms of the GNU General Public
License version 3 or later. For more details, see `COPYING`.

As a special exception to the GNU General Public License, the GDLisp
support file `GDLisp.lisp` can be used and redistributed without any
restrictions.
