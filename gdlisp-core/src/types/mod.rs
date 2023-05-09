
//! Module providing Rust-side rich types for several things
//! GDExtension exposes as untyped dictionaries.
//!
//! All of these types should be convertible to Godot variants via
//! Serde.

pub mod template;
pub mod validate;
