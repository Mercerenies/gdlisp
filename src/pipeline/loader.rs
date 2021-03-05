
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
