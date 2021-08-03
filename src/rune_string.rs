use crate::{
    fss_utf, rune,
    rune_str_ty::{
        rune_str_from_rune_bytes_unchecked, rune_str_from_rune_bytes_unchecked_mut, RuneStr,
    },
    rune_ty::RuneReprCharVec,
};
use std::{fmt, marker::PhantomData, ops, rc::Rc};

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
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_string(s: &str) -> Option<Self> {
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

    pub fn from_string_lossy(s: &str) -> Self {
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

    pub fn push(&mut self, r: rune) {
        let s = r.into_inner();
        let old_len = self.1.len();
        self.1.reserve(fss_utf::MAX_BYTE_COUNT);
        self.1
            .extend(std::iter::repeat(0).take(fss_utf::MAX_BYTE_COUNT));
        unsafe {
            self.1.set_len(old_len);
            let spare_ptr = self.1.as_mut_ptr().offset(old_len as _);
            let used_len = fss_utf::encode_fss_utf(
                s,
                std::slice::from_raw_parts_mut(spare_ptr, fss_utf::MAX_BYTE_COUNT),
            )
            .unwrap();
            self.1.set_len(old_len + used_len);
        }
    }

    pub fn push_rune_str(&mut self, s: &RuneStr) {
        self.1.extend(s.1.iter().copied())
    }
}
