
use std::path::Path;
use std::io::{self, Read, BufReader};
use std::fs::File;

#[derive(Clone, Debug)]
pub struct DefaultNameResolver;

pub trait NameResolver {
  fn resolve_path(&self, filename: &Path) -> io::Result<Box<dyn Read>>;
}

impl NameResolver for DefaultNameResolver {

  fn resolve_path(&self, filename: &Path) -> io::Result<Box<dyn Read>> {
    let input_file = BufReader::new(File::open(filename)?);
    Ok(Box::new(input_file))
  }

}
