
use json::JsonValue;

use std::convert::TryFrom;

/*
 * Responses are received as JSON objects which have the following keys.
 *
 * error_code (required) - 0 if successful, or a Godot error code if
 * failed.
 *
 * error_string (required) - A string of text, possibly empty,
 * specifying more details about the error.
 *
 * response_string (required) - A string of text specifying the
 * response. This string must be empty if an error occurred.
 */

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServerResponse {
  Failure(Failure),
  Success(Success),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Failure {
  pub error_code: u32,
  pub error_string: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Success {
  pub response_string: String,
}

#[derive(Clone, Debug)]
pub enum ParseError {
  MalformedResponse(String),
}

fn fail(json: &JsonValue) -> ParseError {
  ParseError::MalformedResponse(json.to_string())
}

impl TryFrom<JsonValue> for ServerResponse {
  type Error = ParseError;

  fn try_from(json: JsonValue) -> Result<ServerResponse, ParseError> {
    let obj = match &json {
      JsonValue::Object(obj) => obj,
      _ => return Err(fail(&json)),
    };
    let error_code = obj.get("error_code")
      .and_then(|err| err.as_u32())
      .ok_or(fail(&json))?;
    if error_code == 0 {
      // Success case
      let response_string = obj.get("response_string").and_then(|r| r.as_str()).ok_or(fail(&json))?;
      let response_string = response_string.to_owned();
      Ok(ServerResponse::Success(Success { response_string }))
    } else {
      // Failure case
      let error_string = obj.get("error_string").and_then(|r| r.as_str()).ok_or(fail(&json))?;
      let error_string = error_string.to_owned();
      Ok(ServerResponse::Failure(Failure { error_code, error_string }))
    }
  }

}
