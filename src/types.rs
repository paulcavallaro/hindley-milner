use std::fmt;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
  Int64 (i64),
  Str (String),
  Func (Func_),
  Unit,
}

#[derive(Clone)]
pub struct Context {
  pub vars : HashMap<String, Expr>,
}

pub fn mk_ctx() -> Context {
  Context { vars : HashMap::new() }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Func_ {
  pub name : String,
  pub params : Vec<String>,
  pub body : Box<Expr>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
  Val (Value),
  Var (String),
  // Builtins
  Plus (Box<Expr>, Box<Expr>),
  Print (Box<Expr>),
  App (Func_, Vec<Expr>),
}

impl fmt::Display for Value {
  fn fmt(&self, formatter : &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Value::Int64(i) => {
        i.fmt(formatter)
      },
      &Value::Str(ref str) => {
        str.fmt(formatter)
      },
      &Value::Func(ref f) => {
        write!(formatter, "Function {} of arity {}", f.name, f.params.len())
      },
      &Value::Unit =>
      {
        write!(formatter, "()")
      }
    }
  }
}
