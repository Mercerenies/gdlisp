
# GDLisp

Lisp for the [Godot](https://godotengine.org/) platform! This
work-in-progress language aims to be a Lisp dialect which compiles to
GDScript.

This project is built using [Cargo](https://doc.rust-lang.org/cargo/).
Use `cargo test` to run the test suite, and use `cargo run` to compile
stdin input to GDScript.

GDLisp is early in development and is very clearly not ready for actual use.

## Planned Features

* Support for standard GDScript functionality, including function
  declarations, class declarations, signals, etc.
* The expression-based semantics we all love from Lisp
* Lambdas / anonymous functions as well as anonymous classes
* Support for compile-time macros that run in the GDLisp compiler
  itself
