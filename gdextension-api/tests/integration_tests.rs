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

use gdextension_api::load_extension_api;

use std::io::Cursor;

#[test]
pub fn test_extension_api_with_real_file() {
  // Use the real extension_api.json from a production instance of
  // Godot.
  let extension_api_str = include_str!("extension_api.json");
  load_extension_api(Cursor::new(extension_api_str)).unwrap();
}
