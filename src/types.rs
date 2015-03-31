extern crate core;

use self::core::fmt;
use std::collections::HashMap;

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Func_ {
  pub name : String,
  pub params : Vec<String>,
  pub body : Box<Expr>,
}

#[derive(Clone)]
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

pub fn eval(ctx : &Context, expr : Expr) -> Value {
  match expr {
    Expr::Val(v) => v,
    Expr::Var(ref id) =>
    {
      match ctx.vars.get(id) {
        None => panic!("Unbound name {}", id),
        Some(e) => eval(ctx, e.clone()),
      }
    },
    Expr::Plus(e1, e2) =>
    {
      let r1 = eval(ctx, *e1);
      let r2 = eval(ctx, *e2);
      match (r1, r2) {
        (Value::Int64(i1), Value::Int64(i2)) => Value::Int64(i1+i2),
        (l, r) =>
        {
          panic!("Trying to add non integer operand in expression '{} + {}'", l, r)
        }
      }
    },
    Expr::Print(e) =>
    {
      let res = eval(ctx, *e);
      println!("{}", res);
      Value::Unit
    },
    Expr::App(f, args) =>
    {
      let mut new_ctx = ctx.clone();
      if f.params.len() != args.len() {
        panic!("Partial application of function {} of arity {} to {} args",
               f.name, f.params.len(), args.len())
      };
      for (param, arg) in f.params.iter().zip(args.iter()) {
        new_ctx.vars.insert(param.clone(), arg.clone());
      }
      eval(&new_ctx, *f.body)
    },
  }
}