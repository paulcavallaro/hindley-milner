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
  }
}