#[derive(Clone, PartialEq, Debug)]
pub enum Token {
  Let,
  Id,
  Int,
  Plus,
  Eq,
  Fun,
  If,
  Else,
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


*/
