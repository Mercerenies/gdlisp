
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

  pub fn is_empty(&self) -> bool {
    self.args.is_empty()
  }

  pub fn to_gd(&self) -> String {
    self.args.join(", ")
  }

}
