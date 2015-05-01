mod types;
mod interp;
mod infer;

use types::{Value, Expr, Func_, mk_ctx, new_type_ctx};
use interp::eval;
use infer::{expr, new_ident};

fn main() {
  /*
  let ctx = mk_ctx();
  let a = Expr::Val(Value::Int64(64));
  let b = Expr::Val(Value::Int64(65));
  let c = Expr::Plus(Box::new(a.clone()), Box::new(b));
  let mut ty_ctx = new_type_ctx();
  let ty = expr(&mut ty_ctx, a);
  println!("{:?}", ty);
  let ty = expr(&mut ty_ctx, c);
  println!("{:?}", ty);
  let arg_id = new_ident();
  let body = Expr::Plus(Box::new(Expr::Var(arg_id.clone())),
                        Box::new(Expr::Val(Value::Int64(1))));
  let lambda = Expr::Lambda(arg_id.clone(), Box::new(body.clone()));
  let ty = expr(&mut ty_ctx, lambda);
  println!("{:?}", ty);
  let arg_id2 = new_ident();
f x =
  let y = 5 + x in
  x
==>
  lambda x. (lambda y. x) (5 + x)
  */
  let arg_id = new_ident();
  let arg_id2 = new_ident();
  let let_lambda = Expr::Lambda(
    arg_id.clone(),
    Box::new(
      Expr::App(
        Box::new(
          Expr::Lambda(arg_id2.clone(),
                       Box::new(Expr::Var(arg_id.clone())))),
        vec![Expr::Plus(
          Box::new(Expr::Val(Value::Int64(1))),
          Box::new(Expr::Var(arg_id.clone())))])));
  let mut ty_ctx = new_type_ctx();
  let ty = expr(&mut ty_ctx, let_lambda);
  println!("{:?}", ty);

/*
  let b = Expr::Val(Value::Int64(65));
  let c = Expr::Print(Box::new(Expr::Plus(Box::new(a), Box::new(b))));
  let d = eval(&ctx, c);
  let _e = eval(&ctx, Expr::Print(Box::new(Expr::Val(d))));

  let f = Expr::Val(Value::Str("Hello, World!".to_string()));
  let g = eval(&ctx, Expr::Print(Box::new(f)));
  let _h = eval(&ctx, Expr::Print(Box::new(Expr::Val(g))));
//
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
*/
}