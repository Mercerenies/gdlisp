
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum UnaryOp {
  BitNot, Negate, Not,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum BinaryOp {
  Times, Div, Mod, Add, Sub, LShift, RShift, BitAnd, BitXor,
  BitOr, LT, GT, Eq, NE, LE, GE, Is, In, And, Or, Cast,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum AssignOp {
  Eq, Add, Sub, Times, Div, Mod, BitAnd, BitOr,
}

// There is only one ternary op in Godot: if conditionals.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct TernaryOp;

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct OperatorInfo {
  pub precedence: i32,
  pub name: &'static str,
  pub padding: Padding,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub enum Padding {
  NotRequired, Preferred, Required
}

fn info(prec: i32, name: &'static str, padding: Padding) -> OperatorInfo {
  OperatorInfo { precedence: prec, name: name, padding: padding }
}

pub trait OperatorHasInfo {
  fn op_info(&self) -> OperatorInfo;
}

impl OperatorHasInfo for UnaryOp {
  fn op_info(&self) -> OperatorInfo {
    match self {
      UnaryOp::BitNot => info(17, "~", Padding::NotRequired),
      UnaryOp::Negate => info(16, "-", Padding::NotRequired),
      UnaryOp::Not => info(6, "!", Padding::NotRequired),
    }
  }
}

impl OperatorHasInfo for BinaryOp {
  fn op_info(&self) -> OperatorInfo {
    match self {
      BinaryOp::Times => info(15, "*", Padding::Preferred),
      BinaryOp::Div => info(15, "/", Padding::Preferred),
      BinaryOp::Mod => info(15, "%", Padding::Preferred),
      BinaryOp::Add => info(14, "+", Padding::Preferred),
      BinaryOp::Sub => info(13, "-", Padding::Preferred),
      BinaryOp::LShift => info(12, "<<", Padding::Preferred),
      BinaryOp::RShift => info(12, ">>", Padding::Preferred),
      BinaryOp::BitAnd => info(11, "&", Padding::Preferred),
      BinaryOp::BitXor => info(10, "^", Padding::Preferred),
      BinaryOp::BitOr => info(9, "|", Padding::Preferred),
      BinaryOp::LT => info(8, "<", Padding::Preferred),
      BinaryOp::GT => info(8, ">", Padding::Preferred),
      BinaryOp::Eq => info(8, "==", Padding::Preferred),
      BinaryOp::NE => info(8, "!=", Padding::Preferred),
      BinaryOp::LE => info(8, "<=", Padding::Preferred),
      BinaryOp::GE => info(8, ">=", Padding::Preferred),
      BinaryOp::Is => info(18, "is", Padding::Required),
      BinaryOp::In => info(7, "in", Padding::Required),
      BinaryOp::And => info(5, "&&", Padding::Preferred),
      BinaryOp::Or => info(4, "||", Padding::Preferred),
      BinaryOp::Cast => info(2, "as", Padding::Preferred),
    }
  }
}

impl OperatorHasInfo for AssignOp {
  fn op_info(&self) -> OperatorInfo {
    let prec = 1; // All assignments are of the lowest precedence in GDScript
    let pad = Padding::Preferred;
    let name = match self {
      AssignOp::Eq => "=",
      AssignOp::Add => "+=",
      AssignOp::Sub => "-=",
      AssignOp::Times => "*=",
      AssignOp::Div => "/=",
      AssignOp::Mod => "%=",
      AssignOp::BitAnd => "&=",
      AssignOp::BitOr => "|=",
    };
    info(prec, name, pad)
  }
}

impl OperatorHasInfo for TernaryOp {
  fn op_info(&self) -> OperatorInfo {
    info(3, "if-else", Padding::Required)
  }
}
