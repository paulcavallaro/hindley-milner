use utils::interner::{Name};

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
  Invalid,
  Whitespace,
  Eof,
  Let,
  Ident(Name),
  Int(i64),
  Plus,
  Eq,
  Lt,
  Gt,
  LtEq,
  GtEq,
  Fun,
  If,
  Else,
  Then,
}

pub mod keywords {
  pub enum Keyword {
    Let,
    Fun,
    If,
    Else,
    Then,
  }
}

/*

toplevel forms:

let:
let <id> <id...> = <expr>

expr:

let <id> <id...> = <expr>
INT
<expr> + <expr>
if <expr> then <expr> [else <expr>]
(<expr>)

let foo = bar in
let baz = foo + 5 in
baz + baz + foo

*/
