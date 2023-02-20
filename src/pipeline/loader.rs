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

#![deprecated]

use super::TranslationUnit;

use std::path::Path;

pub struct NullFileLoader;

pub struct NullFileLoaderError;

pub trait FileLoader {
  type Error;

  fn load_file<'a, 'b, P>(&'a mut self, input_path: &'b P)
                          -> Result<&'a TranslationUnit, Self::Error>
  where P : AsRef<Path> + ?Sized;

  fn get_file<'a, 'b, P>(&'a self, input_path: &'b P) -> Option<&'a TranslationUnit>
  where P :AsRef<Path> + ?Sized;

}

// Minimal implementation; always results in an error.
impl FileLoader for NullFileLoader {
  type Error = NullFileLoaderError;

  fn load_file<'a, 'b, P>(&'a mut self, _input_path: &'b P)
                          -> Result<&'a TranslationUnit, Self::Error>
  where P : AsRef<Path> + ?Sized {
    Err(NullFileLoaderError)
  }

  fn get_file<'a, 'b, P>(&'a self, _input_path: &'b P) -> Option<&'a TranslationUnit>
  where P :AsRef<Path> + ?Sized {
    None
  }

}
