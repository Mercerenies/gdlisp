
pub mod error;
pub mod ser;

pub use error::{Error, Result};
pub use ser::Serializer;

use serde::Serialize;
use godot::prelude::Variant;

pub fn to_variant<T: Serialize>(value: &T) -> Result<Variant> {
  value.serialize(&Serializer::new())
}
