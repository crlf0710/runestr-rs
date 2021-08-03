use crate::rune_registry::{ControlFlow, THREAD_RUNE_REGISTRY};
use crate::tables::grapheme::GraphemeCat;
use smallvec::smallvec;
use std::{convert::TryFrom, fmt, iter::FromIterator, marker::PhantomData, rc::Rc, str};

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Eq)]
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
        if core_len == 0 {
            return NormalizeFix::InsertSpaceAsCore(precore_len);
        }
        if core_len == 1 {
            if gcat[precore_len] == GraphemeCat::GC_Control {
                return NormalizeFix::None;
            } else if gcat[precore_len] == GraphemeCat::GC_LF {
                return NormalizeFix::None;
            } else if gcat[precore_len] == GraphemeCat::GC_CR {
                return NormalizeFix::InsertLFAfterCRAsCore(precore_len + 1);
            } else if gcat[precore_len] == GraphemeCat::GC_Regional_Indicator {
                return NormalizeFix::RepeatRIAsCore(precore_len, precore_len + 1);
            }
        }
        if core_len == 2 {
            if gcat[precore_len] == GraphemeCat::GC_CR
                && gcat[precore_len + 1] == GraphemeCat::GC_LF
            {
                return NormalizeFix::None;
            } else if gcat[precore_len] == GraphemeCat::GC_Regional_Indicator
                && gcat[precore_len + 1] == GraphemeCat::GC_Regional_Indicator
            {
                return NormalizeFix::None;
            }
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

        return NormalizeFix::None;
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

pub(crate) const MIN_MULTICHAR_RUNE_INTERNAL_VALUE: u32 = char::MAX as u32 + 1;
pub(crate) const MAX_MULTICHAR_RUNE_INTERNAL_VALUE: u32 = 0x7FFFFFFF;

impl rune {
    pub(crate) unsafe fn from_char_unchecked(ch: char) -> Self {
        rune(ch as _, PhantomData)
    }

    pub(crate) unsafe fn from_multi_char_grapheme_cluster_unchecked(repr: &str) -> Self {
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

    pub fn from_char_lossy(ch: char) -> Self {
        use crate::tables::grapheme::grapheme_category;
        use grapheme_analysis::{apply_normalize_fix, normalize_rune};

        let grapheme_cat = [grapheme_category(ch).2];
        let fix = normalize_rune(&grapheme_cat);
        if fix.is_none() {
            unsafe {
                return Self::from_char_unchecked(ch);
            }
        } else {
            let mut lossy_chars: RuneReprCharVec = smallvec![ch];
            apply_normalize_fix(fix, &mut lossy_chars);
            let str = String::from_iter(lossy_chars.into_iter());
            unsafe { Self::from_multi_char_grapheme_cluster_unchecked(&str) }
        }
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
                let str = String::from_iter(repr.into_iter());
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
            let str = String::from_iter(lossy_chars.into_iter());
            unsafe { Self::from_multi_char_grapheme_cluster_unchecked(&str) }
        }
    }

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

    pub(crate) fn from_inner(v: u32) -> Option<Self> {
        if v < MIN_MULTICHAR_RUNE_INTERNAL_VALUE {
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

    pub fn into_inner(self) -> u32 {
        self.0
    }

    pub(crate) unsafe fn from_rune_info(ri: RuneInfo) -> Self {
        match ri {
            RuneInfo::Single(ch) => rune(ch as u32, PhantomData),
            RuneInfo::Multi(idx, _) => rune(
                idx.checked_add(MIN_MULTICHAR_RUNE_INTERNAL_VALUE).unwrap(),
                PhantomData,
            ),
        }
    }

    pub(crate) fn into_rune_info(self) -> RuneInfo {
        if self.0 < MIN_MULTICHAR_RUNE_INTERNAL_VALUE {
            let ch = char::try_from(self.0).expect("Invalid rune internal value");
            RuneInfo::Single(ch)
        } else {
            let idx = self.0 - MIN_MULTICHAR_RUNE_INTERNAL_VALUE;
            RuneInfo::Multi(idx, PhantomData)
        }
    }
}

pub(crate) enum RuneInfo {
    Single(char),
    Multi(u32, PhantomData<Rc<()>>),
}

impl fmt::Display for rune {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.into_rune_info() {
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
            RuneInfo::Single(ch) => {
                write!(f, "{:?}", ch.escape_debug())?;
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
                    write!(f, "{:?}", ch.escape_debug())?;
                }
            }
        }
        write!(f, "')")
    }
}

#[cfg(test)]
mod tests {
    use super::rune;

    macro_rules! str {
        ($($v:expr),* $(,)?) => {
            std::array::IntoIter::new([$($v),*]).collect::<String>()
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
}
