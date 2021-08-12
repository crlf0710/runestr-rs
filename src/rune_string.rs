use crate::{
    fss_utf, rune,
    rune_str_ty::{
        rune_str_from_rune_bytes_unchecked, rune_str_from_rune_bytes_unchecked_mut, RuneStr,
    },
    rune_ty::RuneReprCharVec,
};
use std::{fmt, iter::FromIterator, marker::PhantomData, ops, rc::Rc};

/// A growable rune-based string type.
#[derive(Clone, Default)]
pub struct RuneString(PhantomData<Rc<()>>, Vec<u8>);

impl fmt::Display for RuneString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &**self)
    }
}

impl fmt::Debug for RuneString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &**self)
    }
}

impl ops::Deref for RuneString {
    type Target = RuneStr;

    fn deref(&self) -> &Self::Target {
        unsafe { rune_str_from_rune_bytes_unchecked(&self.1[..]) }
    }
}

impl ops::DerefMut for RuneString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { rune_str_from_rune_bytes_unchecked_mut(&mut self.1[..]) }
    }
}

impl RuneString {
    /// Creates a new empty `RuneString`.
    pub const fn new() -> Self {
        RuneString(PhantomData, Vec::new())
    }

    /// Perform a conversion from `str` to create a `RuneString`,
    /// returning `None` if data needs to be changed.
    ///
    /// You can use `from_str_lossy` instead if you allow the text
    /// be slightly modified to form complete runes.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Option<Self> {
        use unicode_normalization::UnicodeNormalization;
        use unicode_segmentation::UnicodeSegmentation;

        let mut string = RuneString::new();
        for grapheme in s.graphemes(true) {
            let repr = grapheme
                .chars()
                .cjk_compat_variants()
                .nfc()
                .collect::<RuneReprCharVec>();
            let rune = unsafe { rune::from_rune_repr_char_vec(&repr) }?;
            string.push(rune);
        }
        Some(string)
    }

    /// Perform a conversion from `str` to create a `RuneString`,
    /// slightly modifying the text to form complete runes if necessary.
    pub fn from_str_lossy(s: &str) -> Self {
        use unicode_normalization::UnicodeNormalization;
        use unicode_segmentation::UnicodeSegmentation;

        let mut string = RuneString::new();
        for grapheme in s.graphemes(true) {
            let repr = grapheme
                .chars()
                .cjk_compat_variants()
                .nfc()
                .collect::<RuneReprCharVec>();
            let rune = unsafe { rune::from_rune_repr_char_vec_lossy(repr) };
            string.push(rune);
        }
        string
    }

    /// Append the given `rune` to the end of the `RuneString`.
    pub fn push(&mut self, r: rune) {
        let s = r.into_inner();
        let old_len = self.1.len();
        self.1.reserve(fss_utf::MAX_BYTE_COUNT);
        self.1
            .extend(std::iter::repeat(0).take(fss_utf::MAX_BYTE_COUNT));
        unsafe {
            self.1.set_len(old_len);
            let spare_ptr = self.1.as_mut_ptr().add(old_len);
            let used_len = fss_utf::encode_fss_utf(
                s,
                std::slice::from_raw_parts_mut(spare_ptr, fss_utf::MAX_BYTE_COUNT),
            )
            .unwrap();
            self.1.set_len(old_len + used_len);
        }
    }

    /// Append the given `RuneStr` to the end of the `RuneString`.
    pub fn push_rune_str(&mut self, s: &RuneStr) {
        self.1.extend(s.1.iter().copied())
    }
}

impl Extend<rune> for RuneString {
    fn extend<T: IntoIterator<Item = rune>>(&mut self, iter: T) {
        for rune in iter {
            self.push(rune);
        }
    }
}

impl FromIterator<rune> for RuneString {
    fn from_iter<T: IntoIterator<Item = rune>>(iter: T) -> Self {
        let mut runestr = Self::default();
        runestr.extend(iter);
        runestr
    }
}

#[cfg(test)]
mod tests {
    use super::RuneString;

    #[test]
    fn test_rune_count() {
        let runestr1 = RuneString::from_str_lossy("\u{0041}\u{0341}\u{304B}\u{3099}\u{9508}");
        assert_eq!(3, runestr1.runes().count());
        assert_eq!(3, runestr1.chars().count());

        let runestr2 = RuneString::from_str_lossy("\r\r\n\n");
        assert_eq!(3, runestr2.runes().count());
        assert_eq!(5, runestr2.chars().count());
    }
}
