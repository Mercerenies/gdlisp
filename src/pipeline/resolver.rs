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

//! Defines the [`NameResolver`] trait and a suitable [default
//! implementation](DefaultNameResolver).

use std::path::Path;
use std::io::{self, Read, BufReader, Write, BufWriter};
use std::fs::File;

/// A [`NameResolver`] which simply opens the file it is given, for
/// reading or writing as requested.
///
/// `DefaultNameResolver` should be good for 99% of actual use cases.
/// The primary use case for having a non-standard name resolver is
/// during testing, when we want to mock a virtual filesystem rather
/// than importing from the real one where feasible.
#[derive(Clone, Debug)]
pub struct DefaultNameResolver;

/// A [`NameResolver`] which always fails. The resolver methods will
/// unconditionally panic if called on this object.
///
/// This is mainly provided for testing purposes, to assert that a
/// given test case should *not* invoke the name resolver.
#[derive(Clone, Debug)]
pub struct PanickingNameResolver;

/// A `NameResolver` specifies to a [`Pipeline`](super::Pipeline) how
/// it should read and write data before and after the compilation
/// process.
///
/// A `Pipeline` is always given an input pathname of a GDLisp source
/// file to load. From that pathname, a predetermined process
/// translates the name into the output pathname for GDScript code.
/// Then the pipeline's `NameResolver` is queried, to actually open
/// those files.
///
/// For most practical use cases, [`DefaultNameResolver`] is the
/// appropriate name resolver. `DefaultNameResolver` simply opens the
/// files for input and output respectively, without performing any
/// additional transformations. This is provided as a trait to allow
/// dependency injection, primarily for testing purposes. A testing
/// framework can replace the name resolver in order to load files
/// from a virtual testing environment, rather than from the actual
/// hard drive.
pub trait NameResolver {
  /// Open the file with the given filename for reading. An
  /// [`io::Error`] should be signaled if the file does not exist.
  fn resolve_input_path(&self, filename: &Path) -> io::Result<Box<dyn Read>>;
  /// Open the file with the given filename for writing, clearing the
  /// file if it already exists.
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

impl NameResolver for PanickingNameResolver {

  fn resolve_input_path(&self, _filename: &Path) -> io::Result<Box<dyn Read>> {
    panic!("PanickingNameResolver.resolve_input_path")
  }

  fn resolve_output_path(&self, _filename: &Path) -> io::Result<Box<dyn Write>> {
    panic!("PanickingNameResolver.resolve_output_path")
  }

}
