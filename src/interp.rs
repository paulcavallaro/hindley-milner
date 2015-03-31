use types::{Value, Expr, Context};

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
    Expr::Let(id, val, body) =>
    {
      let mut new_ctx = ctx.clone();
      new_ctx.vars.insert(id, *val);
      eval(&new_ctx, *body)
    },
  }
}

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