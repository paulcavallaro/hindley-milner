use types::{Value, Expr, Context, TypingContext, PrimType, Ident, Type};

use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};
use std::collections::HashMap;
static IDENT_CNTR : AtomicUsize = ATOMIC_USIZE_INIT;

pub fn new_ident() -> Ident {
  let cntr = IDENT_CNTR.fetch_add(1, Ordering::Relaxed);
  Ident(cntr)
}

pub fn fresh_tyvar() -> Ident {
  let cntr = IDENT_CNTR.fetch_add(1, Ordering::Relaxed);
  Ident(cntr)
}

pub fn val(val : &Value) -> Type {
  match *val {
    Value::Int64(_) => Type::TPrim(PrimType::Int64),
    Value::Str(_) => Type::TPrim(PrimType::Str),
    Value::Func(_) => panic!("Func type bad"),
    Value::Unit => Type::TUnit,
  }
}

/// Creates a new fresh Type from ty replacing all quantifiers with fresh
/// type variables
fn fresh(ctx : &mut TypingContext,
         to_replace : &mut HashMap<Ident, Ident>,
         ty : &Type) -> Type {
  match *ty {
    Type::TQuant(ref tyvar, ref ty_expr) =>
    {
      to_replace.insert(tyvar.clone(), fresh_tyvar());
      let ty_expr = fresh(ctx, to_replace, &*ty_expr);
      Type::TQuant(tyvar.clone(), Box::new(ty_expr))
    },
    Type::TVar(ref id) =>
    {
      match to_replace.get(&id) {
        None => ty.clone(),
        Some(val) => Type::TVar(val.clone()),
      }
    }
    Type::TPrim(_) =>
    {
      ty.clone()
    },
    Type::TFun(ref arg_ty, ref res_ty) =>
    {
      let arg_ty = fresh(ctx, to_replace, &*arg_ty);
      let res_ty = fresh(ctx, to_replace, &*res_ty);
      Type::TFun(Box::new(arg_ty), Box::new(res_ty))
    },
    Type::TUnit => Type::TUnit,
    Type::TAny => Type::TAny,
  }
}

pub fn var(ctx : &mut TypingContext, id : &Ident) -> Type {
  let ty = match ctx.vars.get(&id) {
    None => panic!("Unbound variable"),
    Some(ty) =>
    {
      ty.clone()
    },
  };
  fresh(ctx, &mut HashMap::new(), &ty)
}

#[test]
fn let_lambda() {
  use types::{Value, Expr, new_type_ctx};
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
  let ety = fully_expand_type(&mut ty_ctx, &ty);
  println!("{:?}", ty);
  println!("{:?}", ety);
  assert_eq!(ety, Type::TFun(Box::new(Type::TPrim(PrimType::Int64)),
                             Box::new(Type::TPrim(PrimType::Int64))));
}

fn add_type(ctx : &mut TypingContext, v : Ident, ty : Type) -> () {
  let v = get_tyvar(ctx, v);
  ctx.tyvars.insert(v, ty);
}

fn rename(ctx : &mut TypingContext, x1 : Ident, x2 : Ident) -> () {
  let x1 = get_tyvar(ctx, x1);
  let x2 = get_tyvar(ctx, x2);
  add_subst(ctx, x1, x2)
}

fn add_subst(ctx : &mut TypingContext, x1 : Ident, x2 : Ident) -> () {
  if x1 != x2 {
    ctx.subst.insert(x1, x2);
  }
}

fn get_tyvar(ctx : &mut TypingContext, x : Ident) -> Ident {
  let mut x_prime = x.clone();
  match ctx.subst.get(&x) {
    None => return x.clone(),
    Some(id) =>
    {
      x_prime = id.clone()
    },
  }
  x_prime = get_tyvar(ctx, x_prime.clone());
  add_subst(ctx, x, x_prime.clone());
  x_prime
}

fn get_type_unsafe(ctx : &mut TypingContext, v : &Ident) -> Type {
  match ctx.tyvars.get(v) {
    None => Type::TAny,
    Some(ty) => ty.clone(),
  }
}

fn fully_expand_type(ctx : &mut TypingContext, ty : &Type) -> Type {
  match *ty {
    Type::TVar(ref id) =>
    {
      expand_type(ctx, ty)
    },
    Type::TFun(ref arg_ty, ref res_ty) =>
    {
      let arg_ty = fully_expand_type(ctx, arg_ty);
      let res_ty = fully_expand_type(ctx, res_ty);
      Type::TFun(Box::new(arg_ty), Box::new(res_ty))
    },
    Type::TPrim(_)
    | Type::TUnit =>
    {
      ty.clone()
    }
    _ =>
    {
      // TODO(ptc) implement
      panic!("unimplemented!")
    }
  }
}

fn expand_type(ctx : &mut TypingContext, ty : &Type) -> Type {
  match *ty {
    Type::TVar(ref id) =>
    {
      // TODO(ptc) this recurses infinitely with unify_
      // Need to return a non type var at some point
      let x = get_tyvar(ctx, id.clone());
      match ctx.tyvars.get(&x) {
        None => Type::TAny,
        Some(ty) => ty.clone()
      }
    },
    _ => ty.clone()
  }
}

pub fn expr(ctx : &mut TypingContext, expr : Expr) -> Type {
  expr_(ctx, expr)
}

fn expr_(ctx : &mut TypingContext, expr : Expr) -> Type {
  println!("==expr_==\nctx: {:?}\nexpr: {:?}", ctx, expr);
  let tmp =
    match expr {
    Expr::Val(v) =>
    {
      val(&v)
    },
    Expr::Var(ref id) =>
    {
      var(ctx, id)
    },
    Expr::Plus(e1, e2) =>
    {
      let ty1 = expr_(ctx, *e1);
      let ty2 = expr_(ctx, *e2);
      unify_(ctx, &ty1, &Type::TPrim(PrimType::Int64));
      unify_(ctx, &ty2, &Type::TPrim(PrimType::Int64));
      Type::TPrim(PrimType::Int64)
    },
    Expr::Print(e) =>
    {
      Type::TUnit
    },
    Expr::Lambda(arg, body) =>
    {
      let tyvar = fresh_tyvar();
      ctx.vars.insert(arg.clone(), Type::TVar(tyvar.clone()));
      let body_ty = expr_(ctx, *body);
      let arg_ty = match ctx.vars.get(&arg) {
        None => panic!("Did not have key"),
        Some(v) => v,
      };
      Type::TFun(Box::new(arg_ty.clone()), Box::new(body_ty))
    },
    Expr::App(f, args) =>
    {
      let fun_ty = expr_(ctx, *f);
      match fun_ty {
        Type::TFun(_,_) => (),
        _ => {
          panic!("Trying to use function application with non function")
        },
      };
      // TODO(ptc) fix multiple arity functions
      let arg = args[0].clone();
      let arg_ty = expr_(ctx, arg);
      let tyvar = fresh_tyvar();
      let tyvar_fun_ty = Type::TFun(Box::new(arg_ty),
                                    Box::new(Type::TVar(tyvar.clone())));
      unify_(ctx, &fun_ty, &tyvar_fun_ty);
      expand_type(ctx, &Type::TVar(tyvar))
    },
    Expr::Let(id, val, body) =>
    {
      Type::TUnit
    },
  };
  println!("==expr_ RESULT==\nctx: {:?}\nty: {:?}", ctx, tmp);
  tmp
}

fn unify_var(ctx : &mut TypingContext, id1 : Ident, id2 : Ident) -> Type {
  let id1 = get_tyvar(ctx, id1);
  let id2 = get_tyvar(ctx, id2);
  if id1 == id2 {
    return Type::TVar(id1)
  }
  let ty1 = get_type_unsafe(ctx, &id1);
  let ty2 = get_type_unsafe(ctx, &id2);
  let fresh_id = fresh_tyvar();
  rename(ctx, id1, fresh_id.clone());
  rename(ctx, id2, fresh_id.clone());
  let ty = unify_(ctx, &ty1, &ty2);
  // TODO(ptc) something to do with recursive types?
  let ty_prime = expand_type(ctx, &ty);
  let ty = unify_(ctx, &ty, &ty_prime);
  add_type(ctx, fresh_id.clone(), ty);
  Type::TVar(fresh_id)
}

fn unify_(ctx : &mut TypingContext, ty1 : &Type, ty2 : &Type) -> Type {
  // TODO(ptc) remove cloning here
  println!("== unifying ==\nctx: {:?}\nty1: {:?}\nty2: {:?}", ctx, ty1, ty2);
  if ty1 == ty2 {
    return ty1.clone()
  }
  let tmp = match (ty1.clone(), ty2.clone()) {
    (Type::TAny, ty)
    | (ty, Type::TAny) => ty,
    (Type::TVar(ref id1), Type::TVar(ref id2)) =>
    {
      unify_var(ctx, id1.clone(), id2.clone())
    },
    (Type::TVar(ref id), ref other_ty)
    | (ref other_ty, Type::TVar(ref id)) =>
    {
      // Get what the type var is currently bound to (ety)
      // Rename the typevar id to a fresh typevar (id')
      // Try to unify ety with the other type, giving ty
      // Bound old id to type ty
      // Return TypeVar (id')
      //
      // TODO(ptc) Figure out
      // Why do things such that we return a TypeVar id' instead of
      // just ty? Is it because as we do futher unifications we
      // need to be able to possible expand out the type of ty'?
      // That seems reasonable/possible with polymorphic, multi-arity functions
      let ety = expand_type(ctx, &Type::TVar(id.clone()));
      let fresh_id = fresh_tyvar();
      rename(ctx, id.clone(), fresh_id.clone());
      let ty = unify_(ctx, &ety, other_ty);
      add_type(ctx, id.clone(), ty);
      Type::TVar(fresh_id)
    },
    (Type::TFun(ref arg_ty1, ref res_ty1), Type::TFun(ref arg_ty2, ref res_ty2)) =>
    {
      let arg_ty = unify_(ctx, arg_ty1, arg_ty2);
      let arg_ty = expand_type(ctx, &arg_ty);
      let res_ty = unify_(ctx, res_ty1, res_ty2);
      let res_ty = expand_type(ctx, &res_ty);
      Type::TFun(Box::new(arg_ty), Box::new(res_ty))
    },
    (Type::TQuant(id1, quant_ty1), Type::TQuant(id2, quant_ty2)) =>
    {
      // TODO(ptc)
      Type::TUnit
    },
    (Type::TQuant(id1, quant_ty1), _) =>
    {
      // TODO(ptc)
      Type::TUnit
    }
    // TODO(ptc)
    _ =>
    {
      panic!("Does not unify")
    }
  };
  println!("== unifying RESULT ==\nctx: {:?}\n ty: {:?}", ctx, tmp);
  tmp
}
