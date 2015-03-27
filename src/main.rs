mod types;

use types::{Value, Expr, eval};

fn main() {
  let a = Expr::Val(Value::Int64(64));
  let b = Expr::Val(Value::Int64(65));
  let c = Expr::Print(Box::new(Expr::Plus(Box::new(a), Box::new(b))));
  let d = eval(c);
  let _e = eval(Expr::Print(Box::new(Expr::Val(d))));

  let f = Expr::Val(Value::Str("Hello, World!".to_string()));
  let g = eval(Expr::Print(Box::new(f)));
  let _h = eval(Expr::Print(Box::new(Expr::Val(g))));
}