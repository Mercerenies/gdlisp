
pub mod error;
pub mod ser;

pub use error::{Error, Result};
pub use ser::Serializer;

use serde::Serialize;
use godot::prelude::Variant;

/// Serializes the Rust value into a Godot variant, using the
/// following rules.
///
/// | Rust type                    | Godot type                | Notes                                    |
/// | ---------------------------- | ------------------------- | ---------------------------------------- |
/// | `bool`                       | `bool`                    |                                          |
/// | `i*`                         | `int`                     |                                          |
/// | `u*`                         | `int`                     | `u64` values will be truncated as needed |
/// | `f32`, `f64`                 | `float`                   |                                          |
/// | `str`                        | `String`                  |                                          |
/// | `char`                       | `String`                  | Serialized as a single-character string  |
/// | `[u8]`                       | `PackedByteArray`         |                                          |
/// | `()`                         | `null`                    |                                          |
/// | `Option<T>`                  | `T` or `null`             |                                          |
/// | `HashMap<K, V>`              | `Dictionary`              |                                          |
/// | `Vec<T>`                     | `Array`                   |                                          |
/// | Tuple                        | `Array`                   |                                          |
/// | Simple enum                  | `String`                  | Serialized as name of enum variant       |
/// | Newtype                      | Underlying type           |                                          |
/// | Single-argument enum variant | Single-element dictionary | Example: `{ "variant_name": ... }`       |
/// | Tuple struct                 | `Array`                   |                                          |
/// | Tuple enum variant           | Singleton dict of `Array` | Example: `{ "variant_name": [ ... ] }`   |
/// | Traditional struct           | `Dictionary`              |                                          |
/// | Struct enum variant          | Dict of dict              | Example: `{ "variant_name": { ... } }`   |
pub fn to_variant<T: Serialize>(value: &T) -> Result<Variant> {
  value.serialize(&Serializer::new())
}
