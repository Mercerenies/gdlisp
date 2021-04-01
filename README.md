
# GDLisp

Lisp for the [Godot](https://godotengine.org/) platform! This
work-in-progress language aims to be a Lisp dialect which compiles to
GDScript.

This project is built using [Cargo](https://doc.rust-lang.org/cargo/).
Use `cargo test` to run the test suite, and use `cargo run` to compile
stdin input to GDScript.

GDLisp is early in development and is very clearly not ready for actual use.

The current version of GDLisp is tested against [Godot 3.3 RC
6](https://godotengine.org/article/release-candidate-godot-3-3-rc-6)
(non-Mono) and expects a command called `godot` to be on your system's
path which points to the Godot executable. It should run fine against
against the current release version 3.2.3, except that the
`--no-window` flag is not supported on some platforms, which results
in awkward blank windows appearing at some points during the
compilation process.

## Planned Features

* Support for standard GDScript functionality, including function
  declarations, class declarations, signals, etc.
* The expression-based semantics we all love from Lisp
* Lambdas / anonymous functions as well as anonymous classes
* Support for compile-time macros that run in the GDLisp compiler
  itself
