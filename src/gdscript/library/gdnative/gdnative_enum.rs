
use serde::{Serialize, Deserialize};

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Enum {
  pub name: String,
  pub values: HashMap<String, i32>,
}
