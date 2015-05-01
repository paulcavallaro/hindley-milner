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
pub struct TypingContext {
  /* Variables (in expressions) to types */
  pub vars : HashMap<Ident, Type>,
  /* TypeVars to actual types */
  pub tyvars : HashMap<Ident, Type>,
  /* Equivalent TypeVars during Unification */
  pub subst : HashMap<Ident, Ident>,
}

pub fn new_type_ctx() -> TypingContext {
  TypingContext {
    vars : HashMap::new(),
    tyvars : HashMap::new(),
    subst : HashMap::new(),
  }
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
  Var (Ident),
  // Builtins
  Plus (Box<Expr>, Box<Expr>),
  Print (Box<Expr>),
  Lambda (Ident, Box<Expr>),
  App (Box<Expr>, Vec<Expr>),
  Let (String, Box<Expr>, Box<Expr>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Ident(pub usize);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum PrimType {
  Int64,
  Str,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
  TAny,
  TPrim (PrimType),
  TVar (Ident),
  TFun (Box<Type>, Box<Type>),
  TQuant (Ident, Box<Type>),
  TUnit,
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
