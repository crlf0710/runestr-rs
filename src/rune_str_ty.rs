use crate::{
    fss_utf,
    rune_registry::THREAD_RUNE_REGISTRY,
    rune_ty::{rune, RuneInfo, RuneReprCharVec},
};
use std::{fmt, marker::PhantomData, mem::transmute, rc::Rc, str};

/// A primitive rune-based string type.
/// It is usally seen in its borrowed form, `&RuneStr`.
pub struct RuneStr(PhantomData<Rc<()>>, pub(crate) [u8]);

impl fmt::Display for RuneStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ch in self.chars() {
            write!(f, "{}", ch)?;
        }
        Ok(())
    }
}

impl fmt::Debug for RuneStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        for ch in self.chars() {
            write!(f, "{:?}", ch.escape_debug())?;
        }
        write!(f, "\"")
    }
}

pub unsafe fn rune_str_from_rune_bytes_unchecked(bytes: &[u8]) -> &RuneStr {
    unsafe { transmute(bytes) }
}

pub unsafe fn rune_str_from_rune_bytes_unchecked_mut(bytes: &mut [u8]) -> &mut RuneStr {
    unsafe { transmute(bytes) }
}

impl RuneStr {
    /// Returns an iterator over the bytes of a rune string slice.
    pub fn bytes(&self) -> Bytes<'_> {
        Bytes { data: &self.1 }
    }

    /// Returns an iterator over the `rune`s of a rune string slice.
    pub fn runes(&self) -> Runes<'_> {
        Runes { data: &self.1 }
    }

    /// Returns an iterator over the `char`s of a rune string slice.
    pub fn chars(&self) -> Chars<'_> {
        Chars {
            marker: PhantomData,
            chars: RuneReprCharVec::default(),
            runes: Some(self.runes()),
            chars_rev: RuneReprCharVec::default(),
        }
    }
}

/// An iterator over the bytes of a rune string slice.
#[derive(Clone, Copy)]
pub struct Bytes<'str> {
    data: &'str [u8],
}

impl<'str> Iterator for Bytes<'str> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        match *self.data {
            [] => None,
            [head, ref rest @ ..] => {
                self.data = rest;
                Some(head)
            }
        }
    }
}

impl<'str> DoubleEndedIterator for Bytes<'str> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match *self.data {
            [] => None,
            [ref rest @ .., tail] => {
                self.data = rest;
                Some(tail)
            }
        }
    }
}

impl fmt::Debug for Bytes<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bytes(")?;
        f.debug_list().entries(self.clone()).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}

/// An iterator over the `rune`s of a rune string slice.
#[derive(Clone, Copy)]
pub struct Runes<'str> {
    data: &'str [u8],
}

impl<'str> Iterator for Runes<'str> {
    type Item = rune;

    fn next(&mut self) -> Option<Self::Item> {
        match *self.data {
            [] => None,
            [head, ref rest @ ..] => {
                let cont_len = fss_utf::cont_len_from_first_byte(head);
                let (cont, rest) = rest.split_at(cont_len);
                let rune_value = fss_utf::decode_fss_utf_value(head, cont);
                self.data = rest;
                Some(rune::from_inner(rune_value).expect("invalid RuneStr data"))
            }
        }
    }
}

impl<'str> DoubleEndedIterator for Runes<'str> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let len = self.data.len();
        if len == 0 {
            return None;
        }

        let cont_actual_len = self
            .data
            .iter()
            .copied()
            .rev()
            .take(crate::fss_utf::MAX_BYTE_COUNT)
            .take_while(|&x| fss_utf::is_cont_byte(x))
            .count();
        let splitpos = len
            .checked_sub(cont_actual_len + 1)
            .expect("invalid RuneStr data");
        let (rest, cur) = self.data.split_at(splitpos);
        match *cur {
            [] => unreachable!(),
            [head, ref cont @ ..] => {
                let cont_len = fss_utf::cont_len_from_first_byte(head);
                if cont_actual_len != cont_len {
                    panic!("invalid RuneStr data");
                }
                let rune_value = fss_utf::decode_fss_utf_value(head, cont);
                self.data = rest;
                Some(rune::from_inner(rune_value).expect("invalid RuneStr data"))
            }
        }
    }
}

impl fmt::Debug for Runes<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Runes(")?;
        f.debug_list().entries(self.clone()).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}

/// An iterator over the `char`s of a rune string slice.
#[derive(Clone)]
pub struct Chars<'str> {
    marker: PhantomData<Rc<()>>,
    chars: RuneReprCharVec,
    runes: Option<Runes<'str>>,
    chars_rev: RuneReprCharVec,
}

impl<'str> Iterator for Chars<'str> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.chars.is_empty() {
            return self.chars.pop();
        }
        if let Some(runes) = &mut self.runes {
            if let Some(r) = runes.next() {
                match r.into_rune_info() {
                    RuneInfo::Single(ch) => {
                        return Some(ch);
                    }
                    RuneInfo::Multi(idx, _) => {
                        let chars = &mut self.chars;
                        THREAD_RUNE_REGISTRY.with(|registry| {
                            registry.with_existing_rune_repr(idx, |bytes| unsafe {
                                chars.extend(str::from_utf8_unchecked(bytes).chars().rev());
                            })
                        });
                        return chars.pop();
                    }
                }
            }
            self.runes = None;
        }
        if !self.chars_rev.is_empty() {
            return Some(self.chars_rev.remove(0));
        }
        None
    }
}

impl<'str> DoubleEndedIterator for Chars<'str> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if !self.chars_rev.is_empty() {
            return self.chars_rev.pop();
        }
        if let Some(runes) = &mut self.runes {
            if let Some(r) = runes.next_back() {
                match r.into_rune_info() {
                    RuneInfo::Single(ch) => {
                        return Some(ch);
                    }
                    RuneInfo::Multi(idx, _) => {
                        let chars_rev = &mut self.chars_rev;
                        THREAD_RUNE_REGISTRY.with(|registry| {
                            registry.with_existing_rune_repr(idx, |bytes| unsafe {
                                chars_rev.extend(str::from_utf8_unchecked(bytes).chars());
                            })
                        });
                        return chars_rev.pop();
                    }
                }
            }
            self.runes = None;
        }
        if !self.chars.is_empty() {
            return Some(self.chars.remove(0));
        }
        None
    }
}

impl fmt::Debug for Chars<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Chars(")?;
        f.debug_list().entries(self.clone()).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}
