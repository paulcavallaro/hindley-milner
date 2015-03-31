extern crate core;

use self::core::fmt;

pub enum Value {
  Int64 (i64),
  Str (String),
  Unit,
}

pub enum Expr {
  Val (Value),
  // Builtins
  Plus (Box<Expr>, Box<Expr>),
  Print (Box<Expr>),
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
      &Value::Unit =>
      {
        write!(formatter, "()");
        Ok(())
      }
    }
  }
}

pub fn eval(expr : Expr) -> Value {
  match expr {
    Expr::Val(v) => v,
    Expr::Plus(e1, e2) =>
    {
      let r1 = eval(*e1);
      let r2 = eval(*e2);
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
      let res = eval(*e);
      match res {
        Value::Int64(i) =>
        {
          println!("{}", i);
          Value::Unit
        },
        Value::Str(str) =>
        {
          println!("{}", str);
          Value::Unit
        }
        Value::Unit =>
        {
          println!("()");
          Value::Unit
        }
      }
    }
  }
}