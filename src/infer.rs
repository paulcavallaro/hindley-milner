use types::{Value, Expr, Context, PolyType, MonoType};

pub fn typeof(expr : Expr) -> PolyType {
  PolyType::Mono(MonoType::Variable(TypeVar { name : "foo" }))
}