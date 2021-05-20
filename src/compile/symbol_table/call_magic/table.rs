
// The compiler maintains a table of known call magic that the user's
// source code is allowed to link up to during compilation. Currently,
// this table is read-only and there are no facilities in GDLisp to
// change it.

// Note that this functionality is used for bootstrapping and there is
// *never* a legitimate reason for an end user to ever need to
// interface with this directly. As such, everything here should be
// regarded as an implementation detail (like anything else in the
// sys/* namespace).

use crate::util::debug_wrapper::DebugWrapper;
use super::CallMagic;

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct MagicTable {
  values: HashMap<String, DebugWrapper<Box<dyn CallMagic + 'static>>>,
}

impl MagicTable {

  pub fn new() -> MagicTable {
    MagicTable::default()
  }

  pub fn get(&self, name: &str) -> Option<&(dyn CallMagic + 'static)> {
    self.values.get(name).map(|x| &*x.0)
  }

  pub fn set(&mut self, name: String, value: Box<dyn CallMagic + 'static>) {
    self.values.insert(name, DebugWrapper(value));
  }

  pub fn del(&mut self, name: &str) {
    self.values.remove(name);
  }

  pub fn into_hashmap(self) -> HashMap<String, Box<dyn CallMagic + 'static>> {
    self.values.into_iter().map(|(k, v)| (k, v.0)).collect()
  }

}

impl From<HashMap<String, Box<dyn CallMagic + 'static>>> for MagicTable {
  fn from(values: HashMap<String, Box<dyn CallMagic + 'static>>) -> MagicTable {
    MagicTable { values: values.into_iter().map(|(k, v)| (k, DebugWrapper(v))).collect() }
  }
}
