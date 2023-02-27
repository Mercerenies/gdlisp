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

pub mod init;
pub mod interface;
mod internal;

const BUILD_CONFIG: Option<&'static str> = option_env!("GDLISP_BUILD_CONFIGURATION");

pub const VALID_BUILD_CONFIGS: [&'static str; 4] = ["float_32", "float_64", "double_32", "double_64"];

pub fn get_build_config() -> &'static str {
  BUILD_CONFIG.unwrap_or("float_64")
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_is_build_config_valid() {
    assert!(VALID_BUILD_CONFIGS.contains(&get_build_config()), "{} is not a valid build configuration. Valid options are float_32, float_64, double_32, double_64", get_build_config());
  }

}
