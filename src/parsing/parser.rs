use std::path::{Path};
use std::io::{Read, Result};
use std::fs::{File};
use std::rc::Rc;
use parsing::ast::{Token};
use utils::interner::{StrInterner, Name};

pub fn lex_file(path : &Path) -> Result<Lexer> {
  let file = try!(File::open(path));
  let lexer = try!(Lexer::new(file));
  Ok(lexer)
}

struct Lexer {
  pub buf : String,
  pub state : LexState,
}

struct LexState {
  pub last_pos : usize,
  pub pos : usize,
  pub cur : Option<char>,
  pub peek_tok : Token,
}

pub fn is_whitespace(c: Option<char>) -> bool {
  let c = match c { Some(c) => c, None => return false};
  match c {
    ' ' | '\n' | '\t' | '\r' => true,
    _ => false,
  }
}

fn ident_start(c : Option<char>) -> bool {
  let c = match c { Some(c) => c, None => return false};
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c == '_'
}

fn ident_continue(c : Option<char>) -> bool {
  let c = match c { Some(c) => c, None => return false};
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c == '_'
}

fn is_dec_digit(c : Option<char>) -> bool {
  let c = match c { Some(c) => c, None => return false};
  c >= '0' && c <= '9'
}

pub fn get_ident_interner() -> Rc<StrInterner> {
    thread_local!(static KEY: Rc<StrInterner> = {
        Rc::new(mk_fresh_interner())
    });
    KEY.with(|k| k.clone())
}

fn intern(s : &str) -> Name {
  get_ident_interner().intern(s)
}

fn mk_fresh_interner() -> StrInterner {
  // Do the same thing as rust and prefill with keywords
  StrInterner::new()
}

impl Lexer {
  fn new(mut file : File) -> Result<Lexer> {
    let mut buf = String::with_capacity(1024);
    try!(file.read_to_string(&mut buf));
    Ok(
      Lexer {
        buf : buf,
        state : LexState {
          last_pos : 0,
          pos : 0,
          cur : Some('\n'),
          peek_tok : Token::Invalid,
        }
      }
    )
  }

  pub fn advance_token(&mut self) {
    match self.scan_whitespace_or_comment() {
      Some(comment) => {
        self.state.peek_tok = comment;
      }
      None => {
        if self.is_eof() {
          self.state.peek_tok = Token::Eof;
        } else {
          self.state.peek_tok = self.next_token_inner();
        }
      }
    }
  }

  fn scan_whitespace_or_comment(&mut self) -> Option<Token> {
    let c = match self.state.cur { Some(c) => c, _ => return None};
    match c {
      c if is_whitespace(Some(c)) => {
        while is_whitespace(self.state.cur) { self.bump(); }
        Some(Token::Whitespace)
      },
      _ => None
    }
  }

  fn next_token_inner(&mut self) -> Token {
    let c = self.state.cur;
    if ident_start(c) {
      let start = self.state.last_pos;
      while ident_continue(self.state.cur) {
        self.bump();
      }
      let ident = self.with_str_from(start, |string| {
        intern(string)
      });
      return Token::Ident(ident)
    }
    if is_dec_digit(c) {
      let num = self.scan_number();
      return Token::Int(num)
    }
    match c.unwrap() {
      '+' => {self.bump(); Token::Plus},
      '=' => {self.bump(); Token::Eq},
      '<' => {self.bump(); Token::Lt},
      '>' => {self.bump(); Token::Gt},
      _ => Token::Invalid,
    }
  }

  fn is_eof(&self) -> bool { self.state.cur.is_none() }

  fn consume_whitespace(&mut self) {
    while is_whitespace(self.state.cur) && !self.is_eof() { self.bump(); }
  }

  fn scan_number(&mut self) -> i64 {
    let start = self.state.last_pos;
    while is_dec_digit(self.state.cur) {
      self.bump();
    }
    let num : i64 = self.with_str_from(start, |string| {
      string.parse()
    }).unwrap();
    return num
  }

  fn with_str_from<T,F>(&self, start : usize, f : F) -> T where
    F: FnOnce(&str) -> T,
  {
    self.with_str_from_to(start, self.state.last_pos, f)
  }

  fn with_str_from_to<T,F>(&self, start : usize, end : usize, f : F) -> T where
    F: FnOnce(&str) -> T,
  {
    f(&self.buf[start..end])
  }

  fn bump(&mut self) {
    self.state.last_pos = self.state.pos;
    if self.state.pos < self.buf.len() {
      let ch = self.buf.char_at(self.state.pos);
      self.state.pos = self.state.pos + ch.len_utf8();
      self.state.cur = Some(ch);
    } else {
      self.state.cur = None;
    }
  }

  fn nextch(&self) -> Option<char> {
    if self.state.pos < self.buf.len() {
      Some(self.buf.char_at(self.state.pos))
    } else {
      None
    }
  }
}
