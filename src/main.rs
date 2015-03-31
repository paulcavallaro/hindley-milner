mod types;
mod interp;

use types::{Value, Expr, Context, Func_};
use interp::eval;
use std::collections::HashMap;

fn main() {
  let ctx = Context { vars : HashMap::new() };
  let a = Expr::Val(Value::Int64(64));
  let b = Expr::Val(Value::Int64(65));
  let c = Expr::Print(Box::new(Expr::Plus(Box::new(a), Box::new(b))));
  let d = eval(&ctx, c);
  let _e = eval(&ctx, Expr::Print(Box::new(Expr::Val(d))));

  let f = Expr::Val(Value::Str("Hello, World!".to_string()));
  let g = eval(&ctx, Expr::Print(Box::new(f)));
  let _h = eval(&ctx, Expr::Print(Box::new(Expr::Val(g))));

  let body = Expr::Plus(Box::new(Expr::Var("i".to_string())),
                        Box::new(Expr::Val(Value::Int64(1))));
  let inc = Func_ { name : "inc".to_string(),
                    params : vec!["i".to_string()],
                    body : Box::new(body),
                  };
  let app = Expr::App(inc, vec![Expr::Val(Value::Int64(20))]);
  let res = eval(&ctx, app);
  let _ = eval(&ctx, Expr::Print(Box::new(Expr::Val(res))));

  let a = Expr::Val(Value::Int64(64));
  let f = Expr::Val(Value::Str("Hello, World!".to_string()));
  let i = eval(&ctx, Expr::Plus(Box::new(a), Box::new(f)));
  let _ = eval(&ctx, Expr::Print(Box::new(Expr::Val(i))));
}