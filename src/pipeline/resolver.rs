
use std::path::Path;
use std::io::{self, Read, BufReader, Write, BufWriter};
use std::fs::File;

// DefaultNameResolver should be good for 99% of actual use cases. The
// primary use case for having a non-standard name resolver is during
// testing, when we want to mock a virtual filesystem rather than
// importing from the real one where feasible.

#[derive(Clone, Debug)]
pub struct DefaultNameResolver;

pub trait NameResolver {
  fn resolve_input_path(&self, filename: &Path) -> io::Result<Box<dyn Read>>;
  fn resolve_output_path(&self, filename: &Path) -> io::Result<Box<dyn Write>>;
}

impl NameResolver for DefaultNameResolver {

  fn resolve_input_path(&self, filename: &Path) -> io::Result<Box<dyn Read>> {
    let input_file = BufReader::new(File::open(filename)?);
    Ok(Box::new(input_file))
  }

  fn resolve_output_path(&self, filename: &Path) -> io::Result<Box<dyn Write>> {
    let output_file = BufWriter::new(File::create(filename)?);
    Ok(Box::new(output_file))
  }

}
