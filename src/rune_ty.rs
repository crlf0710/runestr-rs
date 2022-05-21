use crate::rune_registry::{ControlFlow, THREAD_RUNE_REGISTRY};
use crate::tables::grapheme::GraphemeCat;
use smallvec::smallvec;
use std::{convert::TryFrom, fmt, marker::PhantomData, rc::Rc, str};

/// The `rune` type represents a user-perceived character.
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub struct rune(u32, PhantomData<Rc<()>>);

pub(crate) type RuneReprCharVec = smallvec::SmallVec<[char; 8]>;
pub(crate) type RuneReprGCatVec = smallvec::SmallVec<[GraphemeCat; 8]>;

mod grapheme_analysis {
    use super::GraphemeCat;
    #[derive(Clone, Copy, PartialEq)]
    pub(crate) enum NormalizeFix {
        None,
        InsertSpaceAsCore(usize),
        InsertLFAfterCRAsCore(usize),
        RepeatRIAsCore(usize, usize),
        AppendZWJToPostCore(usize),
        InsertHangulLFillerAndVFiller(usize),
        InsertHangulLFiller(usize),
        InsertHangulVFiller(usize),
    }

    impl NormalizeFix {
        #[inline]
        pub(crate) fn is_none(self) -> bool {
            self == NormalizeFix::None
        }
    }

    pub(crate) fn normalize_rune(gcat: &[GraphemeCat]) -> NormalizeFix {
        let len = gcat.len();
        let precore_len = gcat
            .iter()
            .copied()
            .take_while(|cat| matches!(cat, GraphemeCat::GC_Prepend))
            .count();
        let postcore_len = gcat[precore_len..]
            .iter()
            .rev()
            .copied()
            .take_while(|cat| {
                matches!(
                    cat,
                    GraphemeCat::GC_Extend | GraphemeCat::GC_SpacingMark | GraphemeCat::GC_ZWJ
                )
            })
            .count();
        let core_len = len - precore_len - postcore_len;
        match core_len {
            0 => {
                return NormalizeFix::InsertSpaceAsCore(precore_len);
            }
            1 => {
                if gcat[precore_len] == GraphemeCat::GC_Control
                    || gcat[precore_len] == GraphemeCat::GC_LF
                {
                    return NormalizeFix::None;
                } else if gcat[precore_len] == GraphemeCat::GC_CR {
                    return NormalizeFix::InsertLFAfterCRAsCore(precore_len + 1);
                } else if gcat[precore_len] == GraphemeCat::GC_Regional_Indicator {
                    return NormalizeFix::RepeatRIAsCore(precore_len, precore_len + 1);
                }
            }
            2 => {
                if gcat[precore_len] == GraphemeCat::GC_CR
                    && gcat[precore_len + 1] == GraphemeCat::GC_LF
                    || gcat[precore_len] == GraphemeCat::GC_Regional_Indicator
                        && gcat[precore_len + 1] == GraphemeCat::GC_Regional_Indicator
                {
                    return NormalizeFix::None;
                }
            }
            _ => {}
        }
        let core_gcat_slice = &gcat[precore_len..precore_len + core_len];
        if core_gcat_slice.starts_with(&[GraphemeCat::GC_Extended_Pictographic])
            || core_gcat_slice.ends_with(&[GraphemeCat::GC_Extended_Pictographic])
        {
            // xpicto-sequence core may glue together with next grapheme cluster under certain cases
            let postcore_gcat_slice = &gcat[precore_len + core_len..];
            if postcore_len != 0
                && postcore_gcat_slice
                    .iter()
                    .rev()
                    .copied()
                    .take(1)
                    .all(|cat| cat == GraphemeCat::GC_ZWJ)
                && postcore_gcat_slice
                    .iter()
                    .rev()
                    .copied()
                    .skip(1)
                    .all(|cat| cat == GraphemeCat::GC_Extend)
            {
                return NormalizeFix::AppendZWJToPostCore(precore_len + core_len + postcore_len);
            }
            return NormalizeFix::None;
        }
        if core_gcat_slice.iter().copied().all(|cat| {
            matches!(
                cat,
                GraphemeCat::GC_L
                    | GraphemeCat::GC_LV
                    | GraphemeCat::GC_LVT
                    | GraphemeCat::GC_V
                    | GraphemeCat::GC_T
            )
        }) {
            // hangul-syllable core may glue together with next grapheme cluster
            // this isn't a concern if there's non-empty precore, however we'll
            // normalize it anyway for consistency.
            let pure_l_len = core_gcat_slice
                .iter()
                .copied()
                .take_while(|cat| matches!(cat, GraphemeCat::GC_L))
                .count();
            let pure_lv_len = core_gcat_slice
                .iter()
                .copied()
                .skip(pure_l_len)
                .take_while(|cat| matches!(cat, GraphemeCat::GC_LV))
                .count();
            let pure_v_len = core_gcat_slice
                .iter()
                .copied()
                .skip(pure_l_len + pure_lv_len)
                .take_while(|cat| matches!(cat, GraphemeCat::GC_V))
                .count();
            let pure_lvt_len = core_gcat_slice
                .iter()
                .copied()
                .skip(pure_l_len + pure_lv_len + pure_v_len)
                .take_while(|cat| matches!(cat, GraphemeCat::GC_LVT))
                .count();
            let _pure_t_len = core_gcat_slice
                .iter()
                .copied()
                .skip(pure_l_len + pure_lv_len + pure_v_len + pure_lvt_len)
                .take_while(|cat| matches!(cat, GraphemeCat::GC_T))
                .count();
            let l_filler_needed = (pure_l_len + pure_lv_len + pure_lvt_len) == 0;
            let v_filler_needed = (pure_v_len + pure_lv_len + pure_lvt_len) == 0;
            match (l_filler_needed, v_filler_needed) {
                (true, true) => return NormalizeFix::InsertHangulLFillerAndVFiller(precore_len),
                (true, false) => return NormalizeFix::InsertHangulLFiller(precore_len),
                (false, true) => {
                    return NormalizeFix::InsertHangulVFiller(precore_len + pure_l_len)
                }
                (false, false) => return NormalizeFix::None,
            }
        }

        NormalizeFix::None
    }

    pub(crate) fn apply_normalize_fix(fix: NormalizeFix, repr: &mut super::RuneReprCharVec) {
        match fix {
            NormalizeFix::None => {}
            NormalizeFix::InsertSpaceAsCore(pos) => {
                repr.insert(pos, ' ');
            }
            NormalizeFix::InsertLFAfterCRAsCore(pos) => {
                repr.insert(pos, '\n');
            }
            NormalizeFix::RepeatRIAsCore(source_idx, pos) => {
                let ch = repr[source_idx];
                repr.insert(pos, ch);
            }
            NormalizeFix::AppendZWJToPostCore(pos) => {
                repr.insert(pos, '\u{200D}');
            }
            NormalizeFix::InsertHangulLFillerAndVFiller(pos) => {
                repr.insert_many(pos, ['\u{115F}', '\u{1160}']);
            }
            NormalizeFix::InsertHangulLFiller(pos) => {
                repr.insert(pos, '\u{115F}');
            }
            NormalizeFix::InsertHangulVFiller(pos) => {
                repr.insert(pos, '\u{1160}');
            }
        }
    }
}

pub(crate) const CRLF_RUNE_INTERNAL_VALUE: u32 = b'\r' as u32;

pub(crate) const MIN_MULTICHAR_RUNE_INTERNAL_VALUE: u32 = char::MAX as u32 + 1;
pub(crate) const MAX_MULTICHAR_RUNE_INTERNAL_VALUE: u32 = 0x7FFFFFFF;

impl rune {
    /// Convert a `char` to `rune`, returning `None` if it could not form a complete rune.
    pub fn from_char(ch: char) -> Option<Self> {
        use crate::tables::grapheme::grapheme_category;
        use grapheme_analysis::normalize_rune;

        let grapheme_cat = [grapheme_category(ch).2];
        if normalize_rune(&grapheme_cat).is_none() {
            unsafe { Some(Self::from_char_unchecked(ch)) }
        } else {
            None
        }
    }

    /// Convert a `char` to `rune`, modifying text to form a complete rune if necessary.
    /// See crate documentation for more details on this.
    pub fn from_char_lossy(ch: char) -> Self {
        use crate::tables::grapheme::grapheme_category;
        use grapheme_analysis::{apply_normalize_fix, normalize_rune};

        let grapheme_cat = [grapheme_category(ch).2];
        let fix = normalize_rune(&grapheme_cat);
        if fix.is_none() {
            unsafe { Self::from_char_unchecked(ch) }
        } else {
            let mut lossy_chars: RuneReprCharVec = smallvec![ch];
            apply_normalize_fix(fix, &mut lossy_chars);
            let str = lossy_chars.into_iter().collect::<String>();
            unsafe { Self::from_multi_char_grapheme_cluster_unchecked(&str) }
        }
    }

    pub(crate) unsafe fn from_char_unchecked(ch: char) -> Self {
        rune(ch as _, PhantomData)
    }

    pub(crate) unsafe fn from_multi_char_grapheme_cluster_unchecked(repr: &str) -> Self {
        if repr == "\r\n" {
            return rune(CRLF_RUNE_INTERNAL_VALUE, PhantomData);
        }
        let repr_bytes = repr.as_bytes();
        let registry_idx = THREAD_RUNE_REGISTRY.with(|registry| {
            let find_result = registry.iterate_existing_rune_reprs(|idx, bytes| {
                if bytes == repr_bytes {
                    ControlFlow::Break(idx)
                } else {
                    ControlFlow::Continue(())
                }
            });
            if let Some(idx) = find_result {
                idx
            } else {
                unsafe {
                    registry
                        .append_multichar_rune_repr_unchecked(repr_bytes)
                        .unwrap()
                }
            }
        });
        let internal_idx = registry_idx
            .checked_add(MIN_MULTICHAR_RUNE_INTERNAL_VALUE)
            .unwrap();
        assert!(
            internal_idx <= MAX_MULTICHAR_RUNE_INTERNAL_VALUE,
            "rune internal index used up"
        );
        rune(internal_idx, PhantomData)
    }

    pub(crate) unsafe fn from_rune_repr_char_vec(repr: &RuneReprCharVec) -> Option<Self> {
        use crate::tables::grapheme::grapheme_category;
        use grapheme_analysis::normalize_rune;
        let grapheme_cat = repr
            .iter()
            .copied()
            .map(|ch| grapheme_category(ch).2)
            .collect::<RuneReprGCatVec>();

        if normalize_rune(&grapheme_cat).is_none() {
            if let [ch] = repr[..] {
                unsafe { Some(Self::from_char_unchecked(ch)) }
            } else {
                let str = repr.into_iter().collect::<String>();
                unsafe { Some(Self::from_multi_char_grapheme_cluster_unchecked(&str)) }
            }
        } else {
            None
        }
    }

    pub(crate) unsafe fn from_rune_repr_char_vec_lossy(repr: RuneReprCharVec) -> Self {
        use crate::tables::grapheme::grapheme_category;
        use grapheme_analysis::{apply_normalize_fix, normalize_rune};

        let mut lossy_chars = repr;
        let grapheme_cat = lossy_chars
            .iter()
            .copied()
            .map(|ch| grapheme_category(ch).2)
            .collect::<RuneReprGCatVec>();

        let fix = normalize_rune(&grapheme_cat);
        apply_normalize_fix(fix, &mut lossy_chars);
        if let [ch] = lossy_chars[..] {
            unsafe { Self::from_char_unchecked(ch) }
        } else {
            let str = lossy_chars.into_iter().collect::<String>();
            unsafe { Self::from_multi_char_grapheme_cluster_unchecked(&str) }
        }
    }

    /// Convert a `&str` consisting of a grapheme cluster to `rune`,
    /// returning `None` if the input is not exactly a grapheme cluster or
    /// it could not form a complete rune.
    pub fn from_grapheme_cluster(grapheme: &str) -> Option<Self> {
        use unicode_normalization::UnicodeNormalization;
        use unicode_segmentation::UnicodeSegmentation;

        let mut graphemes = grapheme.graphemes(true);
        let grapheme = graphemes.next()?;
        if graphemes.next().is_some() {
            return None;
        }
        let repr = grapheme
            .chars()
            .cjk_compat_variants()
            .nfc()
            .collect::<RuneReprCharVec>();
        unsafe { Self::from_rune_repr_char_vec(&repr) }
    }

    /// Convert a `&str` consisting of a grapheme cluster to `rune`,
    /// tweaking the text a little if necessary,
    /// or returning `None` if the input is not exactly a grapheme cluster.
    pub fn from_grapheme_cluster_lossy(grapheme: &str) -> Option<Self> {
        use unicode_normalization::UnicodeNormalization;
        use unicode_segmentation::UnicodeSegmentation;

        let mut graphemes = grapheme.graphemes(true);
        let grapheme = graphemes.next()?;
        if graphemes.next().is_some() {
            return None;
        }
        let repr = grapheme
            .chars()
            .cjk_compat_variants()
            .nfc()
            .collect::<RuneReprCharVec>();
        unsafe { Some(Self::from_rune_repr_char_vec_lossy(repr)) }
    }

    /// Constructs a `rune` from its internal representation.
    pub fn from_inner(v: u32) -> Option<Self> {
        if v == CRLF_RUNE_INTERNAL_VALUE {
            Some(rune(CRLF_RUNE_INTERNAL_VALUE, PhantomData))
        } else if v < MIN_MULTICHAR_RUNE_INTERNAL_VALUE {
            let ch = char::try_from(v).ok()?;
            Self::from_char(ch)
        } else {
            let registry_idx = v - MIN_MULTICHAR_RUNE_INTERNAL_VALUE;
            if THREAD_RUNE_REGISTRY.with(|reg| !reg.is_valid_registry_idx(registry_idx)) {
                return None;
            }
            Some(rune(v, PhantomData))
        }
    }

    /// Retrieves the internal representation of this `rune`.
    pub fn into_inner(self) -> u32 {
        self.0
    }

    /// Retrieves the char this rune corresponds to. Returns `None` if it corresponds to the combination of
    /// multiple chars.
    pub fn into_char(self) -> Option<char> {
        match self.into_rune_info() {
            RuneInfo::Single(ch) => Some(ch),
            RuneInfo::CRLF | RuneInfo::Multi(..) => None,
        }
    }

    /// Retrieves an iterator that iterate over the list of the chars this rune corresponds to.
    pub fn into_chars(self) -> Chars {
        match self.into_rune_info() {
            RuneInfo::CRLF => Chars(CharsInner::MultiRev(smallvec!['\r', '\n'])),
            RuneInfo::Single(ch) => Chars(CharsInner::Single(Some(ch))),
            RuneInfo::Multi(idx, _) => {
                let charvec = THREAD_RUNE_REGISTRY.with(|registry| {
                    registry.with_existing_rune_repr(idx, |bytes| unsafe {
                        str::from_utf8_unchecked(bytes)
                            .chars()
                            .rev()
                            .collect::<RuneReprCharVec>()
                    })
                });
                Chars(CharsInner::MultiRev(charvec))
            }
        }
    }

    pub(crate) unsafe fn from_rune_info(ri: RuneInfo) -> Self {
        match ri {
            RuneInfo::CRLF => rune(CRLF_RUNE_INTERNAL_VALUE, PhantomData),
            RuneInfo::Single(ch) => rune(ch as u32, PhantomData),
            RuneInfo::Multi(idx, _) => rune(
                idx.checked_add(MIN_MULTICHAR_RUNE_INTERNAL_VALUE).unwrap(),
                PhantomData,
            ),
        }
    }

    pub(crate) fn into_rune_info(self) -> RuneInfo {
        if self.0 == CRLF_RUNE_INTERNAL_VALUE {
            RuneInfo::CRLF
        } else if self.0 < MIN_MULTICHAR_RUNE_INTERNAL_VALUE {
            let ch = char::try_from(self.0).expect("Invalid rune internal value");
            RuneInfo::Single(ch)
        } else {
            let idx = self.0 - MIN_MULTICHAR_RUNE_INTERNAL_VALUE;
            RuneInfo::Multi(idx, PhantomData)
        }
    }

    /// Returns the number of bytes this char would need if encoded in `RuneStr`.
    /// That number of bytes is always between 1 and 6, inclusive.
    pub fn len_runestr(self) -> usize {
        crate::fss_utf::len(self.0)
    }

    /// Encode this rune as RuneStr into the provided byte buffer,
    /// and then returns the subslice of the buffer that contains the encoded rune.
    ///
    /// # Panics
    ///
    /// Panics if the buffer is not large enough.
    /// A buffer of length six is large enough to encode any `rune`.
    pub fn encode_runestr(self, dst: &mut [u8]) -> &mut crate::RuneStr {
        let len = match crate::fss_utf::encode_fss_utf(self.0, dst) {
            Ok(len) => len,
            _ => panic!(
                "encode_runestr: cannot encode rune, the buffer has {} bytes",
                dst.len()
            ),
        };
        unsafe { crate::rune_str_ty::rune_str_from_rune_bytes_unchecked_mut(&mut dst[..len]) }
    }

    /// Convert a slice of bytes to a rune.
    pub fn from_rune_bytes(bytes: &[u8]) -> Option<Self> {
        match bytes {
            [] => None,
            [h, rest @ ..] => {
                let v = crate::fss_utf::try_decode_fss_utf_value(*h, rest)?;
                Self::from_inner(v)
            }
        }
    }

    /// Returns whether this rune is the CRLF (Carriage-return Line-feed) rune.
    ///
    /// This corresponds to the only ASCII character rune that corresponds to multiple chars.
    pub fn is_crlf(self) -> bool {
        self.0 == CRLF_RUNE_INTERNAL_VALUE
    }
}

pub(crate) enum RuneInfo {
    CRLF,
    Single(char),
    Multi(u32, PhantomData<Rc<()>>),
}

impl fmt::Display for rune {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.into_rune_info() {
            RuneInfo::CRLF => {
                for ch in ['\r', '\n'] {
                    write!(f, "{}", ch)?;
                }
                Ok(())
            }
            RuneInfo::Single(ch) => {
                write!(f, "{}", ch)
            }
            RuneInfo::Multi(idx, _) => {
                let charvec = THREAD_RUNE_REGISTRY.with(|registry| {
                    registry.with_existing_rune_repr(idx, |bytes| unsafe {
                        str::from_utf8_unchecked(bytes)
                            .chars()
                            .collect::<RuneReprCharVec>()
                    })
                });
                for ch in charvec.iter() {
                    write!(f, "{}", ch)?
                }
                Ok(())
            }
        }
    }
}

impl fmt::Debug for rune {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "rune('")?;
        match self.into_rune_info() {
            RuneInfo::CRLF => {
                for ch in ['\r', '\n'] {
                    write!(f, "{}", ch.escape_debug())?;
                }
            }
            RuneInfo::Single(ch) => {
                write!(f, "{}", ch.escape_debug())?;
            }
            RuneInfo::Multi(idx, _) => {
                let charvec = THREAD_RUNE_REGISTRY.with(|registry| {
                    registry.with_existing_rune_repr(idx, |bytes| unsafe {
                        str::from_utf8_unchecked(bytes)
                            .chars()
                            .collect::<RuneReprCharVec>()
                    })
                });
                for ch in charvec.iter() {
                    write!(f, "{}", ch.escape_debug())?;
                }
            }
        }
        write!(f, "')")
    }
}

impl PartialEq<rune> for char {
    fn eq(&self, rhs: &rune) -> bool {
        match rhs.into_rune_info() {
            RuneInfo::Single(ch) => *self == ch,
            RuneInfo::CRLF | RuneInfo::Multi(_, _) => false,
        }
    }
}

impl PartialEq<char> for rune {
    fn eq(&self, rhs: &char) -> bool {
        match self.into_rune_info() {
            RuneInfo::Single(ch) => ch == *rhs,
            RuneInfo::CRLF | RuneInfo::Multi(_, _) => false,
        }
    }
}

/// An iterator over the `char`s of a rune.
#[derive(Clone)]
pub struct Chars(CharsInner);

#[derive(Clone)]
enum CharsInner {
    Single(Option<char>),
    MultiRev(RuneReprCharVec),
}

impl fmt::Debug for Chars {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl Iterator for Chars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            CharsInner::Single(opt_ch) => opt_ch.take(),
            CharsInner::MultiRev(char_vec_rev) => char_vec_rev.pop(),
        }
    }
}

impl DoubleEndedIterator for Chars {
    fn next_back(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            CharsInner::Single(opt_ch) => opt_ch.take(),
            CharsInner::MultiRev(char_vec_rev) => {
                if char_vec_rev.is_empty() {
                    None
                } else {
                    Some(char_vec_rev.remove(0))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::rune;

    macro_rules! str {
        ($($v:expr),* $(,)?) => {
            [$($v),*].into_iter().collect::<String>()
        };
    }

    #[test]
    fn test_display() {
        assert_eq!(" ", &rune::from_char(' ').unwrap().to_string());
        assert_eq!("\r\n", &rune::from_char_lossy('\r').to_string());
        assert_eq!(
            "\u{00E1}", // á
            &rune::from_grapheme_cluster(&str!['\x61', '\u{0341}'])
                .unwrap()
                .to_string()
        );
        assert_eq!(
            "\u{1100}\u{1160}", // ᄀᅠ
            &rune::from_grapheme_cluster_lossy(&str!['\u{1100}'])
                .unwrap()
                .to_string()
        );
        assert_eq!(
            "\u{1100}\u{1100}\u{1160}", // ᄀᄀᅠ
            &rune::from_grapheme_cluster_lossy(&str!['\u{1100}', '\u{1100}'])
                .unwrap()
                .to_string()
        );
    }

    #[test]
    fn test_crlf_inner_is_ascii_cr() {
        assert_eq!(None, rune::from_char('\r'));
        assert_eq!(b'\r' as u32, rune::from_char_lossy('\r').into_inner());
        assert!(rune::from_char_lossy('\r').is_crlf())
    }

    #[test]
    fn test_debug_fmt_rune() {
        assert_eq!(
            "rune('\\r\\n')",
            format!("{:?}", rune::from_char_lossy('\r'))
        );
        assert_eq!("rune('A')", format!("{:?}", rune::from_char_lossy('A')));
    }
}
