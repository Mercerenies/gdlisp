
// For now, this is just a vector. But I do plan to add support for
// default arguments at some point in the future, so I want to go
// ahead and have it be in a separate file with some abstractions on
// it, so we can update it easily.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgList {
  pub args: Vec<String>,
}

// TODO Support default arguments

impl ArgList {

  pub fn empty() -> ArgList {
    ArgList { args: vec!() }
  }

  pub fn required(args: Vec<String>) -> ArgList {
    ArgList { args }
  }

  pub fn to_gd(&self) -> String {
    self.args.join(", ")
  }

}
