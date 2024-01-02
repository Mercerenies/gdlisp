
//! Serializer which takes Rust types to Godot types.
//!
//! See the documentation for [`crate::to_variant`] for more
//! information on the specific translations made.

use crate::error::{Result, Error};

use serde::ser;
use godot::prelude::{Variant, PackedByteArray, Dictionary, Array, ToGodot};

use std::marker::PhantomData;

#[derive(Default)]
pub struct Serializer {
  private: PhantomData<()>,
}

pub struct SerializeSeq {
  serializer: Serializer,
  builder: Array<Variant>,
}

pub struct SerializeTupleVariant {
  name: &'static str,
  seq: SerializeSeq,
}

pub struct SerializeMap {
  serializer: Serializer,
  builder: Dictionary,
  current_key: Option<Variant>,
}

pub struct SerializeStruct {
  serializer: Serializer,
  builder: Dictionary,
}

pub struct SerializeStructVariant {
  name: &'static str,
  seq: SerializeStruct,
}

impl Serializer {
  pub fn new() -> Serializer {
    Serializer {
      private: PhantomData,
    }
  }
}

impl SerializeSeq {
  fn new() -> SerializeSeq {
    SerializeSeq {
      serializer: Serializer::new(),
      builder: Array::new(),
    }
  }
}

impl SerializeTupleVariant {
  fn new(name: &'static str) -> SerializeTupleVariant {
    SerializeTupleVariant {
      name,
      seq: SerializeSeq::new(),
    }
  }
}

impl SerializeMap {
  fn new() -> SerializeMap {
    SerializeMap {
      serializer: Serializer::new(),
      builder: Dictionary::new(),
      current_key: None,
    }
  }
}

impl SerializeStruct {
  fn new() -> SerializeStruct {
    SerializeStruct {
      serializer: Serializer::new(),
      builder: Dictionary::new(),
    }
  }
}

impl SerializeStructVariant {
  fn new(name: &'static str) -> SerializeStructVariant {
    SerializeStructVariant {
      name,
      seq: SerializeStruct::new(),
    }
  }
}

impl<'a> ser::Serializer for &'a Serializer {

  type Ok = Variant;
  type Error = Error;

  type SerializeSeq = SerializeSeq;
  type SerializeTuple = SerializeSeq;
  type SerializeTupleStruct = SerializeSeq;
  type SerializeTupleVariant = SerializeTupleVariant;
  type SerializeMap = SerializeMap;
  type SerializeStruct = SerializeStruct;
  type SerializeStructVariant = SerializeStructVariant;

  fn serialize_bool(self, v: bool) -> Result<Variant> {
    Ok(Variant::from(v))
  }

  fn serialize_i8(self, v: i8) -> Result<Variant> {
    self.serialize_i64(i64::from(v))
  }

  fn serialize_i16(self, v: i16) -> Result<Variant> {
    self.serialize_i64(i64::from(v))
  }

  fn serialize_i32(self, v: i32) -> Result<Variant> {
    self.serialize_i64(i64::from(v))
  }

  fn serialize_i64(self, v: i64) -> Result<Variant> {
    Ok(Variant::from(v))
  }

  fn serialize_u8(self, v: u8) -> Result<Variant> {
    self.serialize_i64(i64::from(v))
  }

  fn serialize_u16(self, v: u16) -> Result<Variant> {
    self.serialize_i64(i64::from(v))
  }

  fn serialize_u32(self, v: u32) -> Result<Variant> {
    self.serialize_i64(i64::from(v))
  }

  fn serialize_u64(self, v: u64) -> Result<Variant> {
    // Truncate u64 to i64.
    self.serialize_i64(v as i64)
  }

  fn serialize_f32(self, v: f32) -> Result<Variant> {
    self.serialize_f64(f64::from(v))
  }

  fn serialize_f64(self, v: f64) -> Result<Variant> {
    Ok(Variant::from(v))
  }

  fn serialize_char(self, v: char) -> Result<Variant> {
    self.serialize_str(&v.to_string())
  }

  fn serialize_str(self, v: &str) -> Result<Variant> {
    Ok(Variant::from(v))
  }

  fn serialize_bytes(self, v: &[u8]) -> Result<Variant> {
    let byte_array = PackedByteArray::from(v);
    Ok(Variant::from(byte_array))
  }

  fn serialize_none(self) -> Result<Variant> {
    Ok(Variant::nil())
  }

  fn serialize_some<T>(self, value: &T) -> Result<Variant>
  where T: ?Sized + ser::Serialize {
    value.serialize(self)
  }

  fn serialize_unit(self) -> Result<Variant> {
    Ok(Variant::nil())
  }

  fn serialize_unit_struct(self, _name: &'static str) -> Result<Variant> {
    Ok(Variant::nil())
  }

  fn serialize_unit_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
  ) -> Result<Variant> {
    self.serialize_str(variant)
  }

  fn serialize_newtype_struct<T>(
    self,
    _name: &'static str,
    value: &T,
  ) -> Result<Variant>
  where T: ?Sized + ser::Serialize,
  {
    value.serialize(self)
  }

  fn serialize_newtype_variant<T>(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    value: &T,
  ) -> Result<Variant>
  where T: ?Sized + ser::Serialize,
  {
    let dict = singleton_dict(variant, value.serialize(self)?);
    Ok(Variant::from(dict))
  }

  fn serialize_seq(self, _len: Option<usize>) -> Result<SerializeSeq> {
    Ok(SerializeSeq::new())
  }

  fn serialize_tuple(self, len: usize) -> Result<SerializeSeq> {
    self.serialize_seq(Some(len))
  }

  fn serialize_tuple_struct(self, _name: &'static str, len: usize) -> Result<SerializeSeq> {
    self.serialize_seq(Some(len))
  }

  fn serialize_tuple_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    _len: usize,
  ) -> Result<SerializeTupleVariant>
  {
    Ok(SerializeTupleVariant::new(variant))
  }

  fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
    Ok(SerializeMap::new())
  }

  fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
    Ok(SerializeStruct::new())
  }

  fn serialize_struct_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    variant: &'static str,
    _len: usize,
  ) -> Result<Self::SerializeStructVariant> {
    Ok(SerializeStructVariant::new(variant))
  }

}

impl ser::SerializeSeq for SerializeSeq {
  type Ok = Variant;
  type Error = Error;

  fn serialize_element<T>(&mut self, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    self.builder.push(value.serialize(&self.serializer)?);
    Ok(())
  }

  fn end(self) -> Result<Variant> {
    Ok(Variant::from(self.builder))
  }

}

impl ser::SerializeTuple for SerializeSeq {
  type Ok = Variant;
  type Error = Error;

  fn serialize_element<T>(&mut self, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    self.builder.push(value.serialize(&self.serializer)?);
    Ok(())
  }

  fn end(self) -> Result<Variant> {
    Ok(Variant::from(self.builder))
  }

}

impl ser::SerializeTupleStruct for SerializeSeq {
  type Ok = Variant;
  type Error = Error;

  fn serialize_field<T>(&mut self, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    self.builder.push(value.serialize(&self.serializer)?);
    Ok(())
  }

  fn end(self) -> Result<Variant> {
    Ok(Variant::from(self.builder))
  }

}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
  type Ok = Variant;
  type Error = Error;

  fn serialize_field<T>(&mut self, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    ser::SerializeSeq::serialize_element(&mut self.seq, value)
  }

  fn end(self) -> Result<Variant> {
    let arr = Variant::from(self.seq.builder);
    let dict = singleton_dict(self.name, arr);
    Ok(Variant::from(dict))
  }

}

impl ser::SerializeMap for SerializeMap {
  type Ok = Variant;
  type Error = Error;

  fn serialize_key<T>(&mut self, key: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    assert!(self.current_key.is_none(), "Expected serialize_value after serialize_key");
    self.current_key = Some(key.serialize(&self.serializer)?);
    Ok(())
  }

  fn serialize_value<T>(&mut self, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    let key = self.current_key.take().expect("Expected serialize_key before serialize_value");
    let value = value.serialize(&self.serializer)?;
    self.builder.insert(key, value);
    Ok(())
  }

  fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<()>
  where K: ?Sized + ser::Serialize,
        V: ?Sized + ser::Serialize {
    assert!(self.current_key.is_none(), "Incomplete key in SerializeMap::serialize_entry");
    let key = key.serialize(&self.serializer)?;
    let value = value.serialize(&self.serializer)?;
    self.builder.insert(key, value);
    Ok(())
  }

  fn end(self) -> Result<Variant> {
    assert!(self.current_key.is_none(), "Incomplete key in SerializeMap::end");
    Ok(Variant::from(self.builder))
  }

}

impl ser::SerializeStruct for SerializeStruct {
  type Ok = Variant;
  type Error = Error;

  fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    let value = value.serialize(&self.serializer)?;
    self.builder.insert(key, value);
    Ok(())
  }

  fn end(self) -> Result<Variant> {
    Ok(Variant::from(self.builder))
  }

}

impl ser::SerializeStructVariant for SerializeStructVariant {
  type Ok = Variant;
  type Error = Error;

  fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
  where T: ?Sized + ser::Serialize {
    ser::SerializeStruct::serialize_field(&mut self.seq, key, value)
  }

  fn end(self) -> Result<Variant> {
    let inner_dict = Variant::from(self.seq.builder);
    let outer_dict = singleton_dict(self.name, inner_dict);
    Ok(Variant::from(outer_dict))
  }

}

fn singleton_dict<K, V>(key: K, value: V) -> Dictionary
where K: ToGodot,
      V: ToGodot {
  let mut dict = Dictionary::new();
  dict.insert(key, value);
  dict
}
