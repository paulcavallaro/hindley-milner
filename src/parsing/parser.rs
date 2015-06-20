use std::path::{Path};
use std::io::{Read, Result};
use std::fs::{File};
use parsing::ast::{Token};

fn lex_file(path : &Path) -> Result<Lexer> {
  let file = try!(File::open(path));
  let lexer = try!(Lexer::new(file));
  Ok(lexer)
}

struct Lexer {
  buf : String,
  state : LexState,
}

struct LexState {
  pos : usize,
}

impl Lexer {
  fn new(mut file : File) -> Result<Lexer> {
    let mut buf = String::with_capacity(1024);
    try!(file.read_to_string(&mut buf));
    Ok(
      Lexer {
        buf : buf,
        state : LexState {
          pos : 0,
        }
      }
    )
  }

  fn parse(&self) -> Token {
    Token::Let
  }
}
