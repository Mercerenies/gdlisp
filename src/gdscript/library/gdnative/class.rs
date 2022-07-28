
use super::api_type::ApiType;

use serde::{Serialize, Deserialize};

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Class {
  pub name: String,
  pub base_class: String,
  pub api_type: ApiType,
  pub singleton: bool,
  pub singleton_name: String,
  pub instanciable: bool,
  pub is_reference: bool,
  pub constants: HashMap<String, i32>,
}
