// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Types for representing the success or failure of a
//! [`ServerCommand`](super::command::ServerCommand).

use json::JsonValue;

use std::convert::TryFrom;
use std::fmt;
use std::error::Error;

/// A response received from the server as the result of a command.
///
/// `ServerResponse` objects are typically constructed using
/// [`TryFrom::<JsonValue>`] on the actual JSON data received from the
/// server.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ServerResponse {
  Failure(Failure),
  Success(Success),
}

/// A negative response, indicating that something went wrong during
/// command evaluation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Failure {
  /// A [Godot error
  /// code](https://docs.godotengine.org/en/stable/classes/class_%40globalscope.html#class-globalscope-constant-ok)
  /// indicating what went wrong.
  pub error_code: u32,
  /// A user-friendly string detailing the error.
  pub error_string: String,
}

/// A positive response, indicating successful completion of the
/// command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Success {
  /// A string detailing the response. The exact format and use of
  /// this string is entirely dependent on the command issued.
  pub response_string: String,
}

/// An error during parsing of a [`JsonValue`] as a
/// [`ServerResponse`].
#[derive(Clone, Debug)]
pub enum ParseError {
  MalformedResponse(String),
}

fn fail(json: &JsonValue) -> ParseError {
  ParseError::MalformedResponse(json.to_string())
}

impl TryFrom<ServerResponse> for Success {
  type Error = Failure;

  fn try_from(resp: ServerResponse) -> Result<Success, Failure> {
    match resp {
      ServerResponse::Failure(err) => Err(err),
      ServerResponse::Success(val) => Ok(val),
    }
  }

}

impl TryFrom<ServerResponse> for String {
  type Error = Failure;

  fn try_from(resp: ServerResponse) -> Result<String, Failure> {
    let Success { response_string } = Success::try_from(resp)?;
    Ok(response_string)
  }

}

/// Responses are received as JSON objects which have the following keys.
///
/// * error_code (required) - 0 if successful, or a Godot error code
/// if failed.
///
/// * error_string (required) - A string of text, possibly empty,
/// specifying more details about the error.
///
/// response_string (required) - A string of text specifying the
/// response. This string must be empty if an error occurred.
impl TryFrom<JsonValue> for ServerResponse {
  type Error = ParseError;

  fn try_from(json: JsonValue) -> Result<ServerResponse, ParseError> {
    let obj = match &json {
      JsonValue::Object(obj) => obj,
      _ => return Err(fail(&json)),
    };
    let error_code = obj.get("error_code")
      .and_then(|err| err.as_u32())
      .ok_or_else(|| fail(&json))?;
    if error_code == 0 {
      // Success case
      let response_string = obj.get("response_string").and_then(|r| r.as_str()).ok_or_else(|| fail(&json))?;
      let response_string = response_string.to_owned();
      Ok(ServerResponse::Success(Success { response_string }))
    } else {
      // Failure case
      let error_string = obj.get("error_string").and_then(|r| r.as_str()).ok_or_else(|| fail(&json))?;
      let error_string = error_string.to_owned();
      Ok(ServerResponse::Failure(Failure { error_code, error_string }))
    }
  }

}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ParseError::MalformedResponse(s) => {
        writeln!(f, "{}", s)
      }
    }
  }
}

impl fmt::Display for Failure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "{} {}", self.error_code, self.error_string)
  }
}

impl Error for Failure {}
