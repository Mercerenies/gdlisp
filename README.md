[![GitHub Actions: Ubuntu](https://github.com/mercerenies/gdlisp/actions/workflows/test-Godotv3.4-Ubuntu.yml/badge.svg)](https://github.com/Mercerenies/gdlisp/actions/workflows/test-Godotv3.4-Ubuntu.yml)
[![GitHub Actions: Ubuntu](https://github.com/mercerenies/gdlisp/actions/workflows/test-Godotv3.3.3-Ubuntu.yml/badge.svg)](https://github.com/Mercerenies/gdlisp/actions/workflows/test-Godotv3.3.3-Ubuntu.yml)

# GDLisp

Lisp for the [Godot](https://godotengine.org/) platform! This
work-in-progress language aims to be a Lisp dialect which compiles to
GDScript.

This project is built using [Cargo](https://doc.rust-lang.org/cargo/).
Use `cargo test` to run the test suite, and use `cargo run` to compile
stdin input to GDScript.

GDLisp is early in development and is very clearly not ready for actual use.

The current version of GDLisp is tested against [Godot
3.3.3](https://godotengine.org/article/maintenance-release-godot-3-3-3)
(non-Mono) and expects a command called `godot` to be on your system's
path which points to the Godot executable.

## Planned Features

* Support for standard GDScript functionality, including function
  declarations, class declarations, signals, etc.
* The expression-based semantics we all love from Lisp
* Lambdas / anonymous functions as well as anonymous classes
* Support for compile-time macros that run in the GDLisp compiler
  itself
