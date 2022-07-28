
use super::argument::Argument;

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Signal {
  pub name: String,
  pub arguments: Vec<Argument>,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn deserialize_signal_test() {
    let signal_str = r#"{
				"name": "mesh_updated",
				"arguments": [
					{
						"name": "mesh",
						"type": "Mesh",
						"has_default_value": false,
						"default_value": ""
					}
				]
			}"#;
    let result: Signal = serde_json::from_str(signal_str).unwrap();
    assert_eq!(result, Signal {
      name: String::from("mesh_updated"),
      arguments: vec![
        Argument {
          name: String::from("mesh"),
          argument_type: String::from("Mesh"),
          has_default_value: false,
          default_value: String::from(""),
        },
      ],
    });
  }

}
