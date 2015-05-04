mod types;
mod infer;

use types::{Value, Expr, new_type_ctx};
use infer::{expr, new_ident, fully_expand_type};
use std::collections::{HashSet};

fn main() {
/*
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
  let ety = fully_expand_type(&mut ty_ctx, &mut HashSet::new(), &ty);
  println!("ty: {:?}", ty);
  println!("ety: {:?}", ety);
/*
  let f x = x
*/
  let arg_id = new_ident();
  let id = Expr::Lambda(arg_id.clone(),
                                Box::new(Expr::Var(arg_id.clone())));
  ty_ctx = new_type_ctx();
  let ty = expr(&mut ty_ctx, id);
  let ety = fully_expand_type(&mut ty_ctx, &mut HashSet::new(), &ty);
  println!("ty: {:?}", ty);
  println!("ety: {:?}", ety);
}