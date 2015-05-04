use types::{Value, Expr};

pub fn eval(expr : Expr) -> Unit {
}

/*
#[test]
fn addition() {
  use types::mk_ctx;
  let ten = Expr::Val(Value::Int64(10));
  let add = Expr::Plus(Box::new(ten.clone()), Box::new(ten));
  let ctx = mk_ctx();
  assert_eq!(eval(&ctx, add), Value::Int64(20));
}

#[test]
#[should_fail(expected = "non integer operand")]
fn bad_add() {
  use types::mk_ctx;
  let ten = Expr::Val(Value::Int64(10));
  let str = Expr::Val(Value::Str("Hello!".to_string()));
  let add = Expr::Plus(Box::new(ten), Box::new(str));
  let ctx = mk_ctx();
  eval(&ctx, add);
}

#[test]
fn let_var() {
  use types::mk_ctx;
  let let_expr = Expr::Let("foo".to_string(),
                           Box::new(Expr::Val(Value::Int64(10))),
                           Box::new(Expr::Var("foo".to_string())));
  let ctx = mk_ctx();
  assert_eq!(eval(&ctx, let_expr), Value::Int64(10));
}
*/