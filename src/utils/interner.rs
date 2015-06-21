use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap};
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Eq, Ord, PartialEq, PartialOrd, Hash,
         Clone, Copy, Debug)]
pub struct Name(pub u32);

impl Name {
    pub fn usize(&self) -> usize {
        let Name(nm) = *self;
        nm as usize
    }
}

#[derive(Clone, PartialEq, Hash, PartialOrd)]
pub struct RcStr {
    string: Rc<String>,
}

impl RcStr {
    pub fn new(string: &str) -> RcStr {
        RcStr {
            string: Rc::new(string.to_string()),
        }
    }
}

impl Eq for RcStr {}

impl Ord for RcStr {
    fn cmp(&self, other: &RcStr) -> Ordering {
        self[..].cmp(&other[..])
    }
}

impl fmt::Debug for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Debug;
        self[..].fmt(f)
    }
}

impl fmt::Display for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Display;
        self[..].fmt(f)
    }
}

impl Borrow<str> for RcStr {
    fn borrow(&self) -> &str {
        &self.string[..]
    }
}

impl Deref for RcStr {
    type Target = str;

    fn deref(&self) -> &str { &self.string[..] }
}

pub struct StrInterner {
  map: RefCell<HashMap<RcStr, Name>>,
  vect: RefCell<Vec<RcStr> >,
}

/// When traits can extend traits, we should extend index<Name,T> to get []
impl StrInterner {
  pub fn new() -> StrInterner {
    StrInterner {
      map: RefCell::new(HashMap::new()),
      vect: RefCell::new(Vec::new()),
    }
  }

  pub fn prefill(init: &[&str]) -> StrInterner {
    let rv = StrInterner::new();
    for &v in init { rv.intern(v); }
    rv
  }

  pub fn intern(&self, val: &str) -> Name {
    let mut map = self.map.borrow_mut();
    match map.get(val) {
      Some(&idx) => return idx,
      None => (),
    }

    let new_idx = Name(self.len() as u32);
    let val = RcStr::new(val);
    map.insert(val.clone(), new_idx);
    self.vect.borrow_mut().push(val);
    new_idx
  }

  pub fn gensym(&self, val: &str) -> Name {
    let new_idx = Name(self.len() as u32);
    // leave out of .map to avoid colliding
    self.vect.borrow_mut().push(RcStr::new(val));
    new_idx
  }

  // I want these gensyms to share name pointers
  // with existing entries. This would be automatic,
  // except that the existing gensym creates its
  // own managed ptr using to_managed. I think that
  // adding this utility function is the most
  // lightweight way to get what I want, though not
  // necessarily the cleanest.

  /// Create a gensym with the same name as an existing
  /// entry.
  pub fn gensym_copy(&self, idx : Name) -> Name {
    let new_idx = Name(self.len() as u32);
    // leave out of map to avoid colliding
    let mut vect = self.vect.borrow_mut();
    let existing = (*vect)[idx.usize()].clone();
    vect.push(existing);
    new_idx
  }

  pub fn get(&self, idx: Name) -> RcStr {
    (*self.vect.borrow())[idx.usize()].clone()
  }

  pub fn len(&self) -> usize {
    self.vect.borrow().len()
  }

  pub fn find<Q: ?Sized>(&self, val: &Q) -> Option<Name>
    where RcStr: Borrow<Q>, Q: Eq + Hash {
      match (*self.map.borrow()).get(val) {
        Some(v) => Some(*v),
        None => None,
      }
    }

  pub fn clear(&self) {
    *self.map.borrow_mut() = HashMap::new();
    *self.vect.borrow_mut() = Vec::new();
  }

  pub fn reset(&self, other: StrInterner) {
    *self.map.borrow_mut() = other.map.into_inner();
    *self.vect.borrow_mut() = other.vect.into_inner();
  }
}