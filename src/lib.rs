#![deny(unsafe_op_in_unsafe_fn)]
#![deny(warnings, missing_docs, missing_debug_implementations)]
#![allow(irrefutable_let_patterns, dead_code)]
//! User-perceived characters related types and data structures.
//!
//! The `rune` type represents a user-perceived character. It roughly corresponds to a Unicode grapheme cluster
//! but with some nice properties. Runes that consists of two or more `char`s are automatically registered on a
//! per-thread basis. This also means that `rune`s are neither `Send` nor `Sync`.
//!
//! The `RuneStr` type, also called a "rune string slice", is a primitive rune-based string type.
//! It is usally seen in its borrowed form, `&RuneStr`.
//!
//! Rune string slices are encoded in a special encoding called `FSS-UTF`, which is a super-set of UTF-8 encoding.
//! This allows all `rune`s be encoded.
//!
//! The `RuneString` type, is a growable rune-based string type.
//!
//! # Rune definition
//!
//! Our rune definition is based on the extended grapheme cluster defined within UAX-29.
//! On top of this, we will convert all the CJK Compatibility Ideographs to their equivalent IVS form,
//! and then convert the text to NFC form. We also apply a few specfic "lossy conversion" rules when
//! necessary. The rules are defined below, and their goal to make each of the rune "standalone", that is,
//! when two runes are put next to one each other, they won't automatically merge together into one larger
//! rune.
//!
//! # Rules for lossy conversion within a rune
//!
//! * An orphan abstract character CR (U+000D) is converted into CR LF sequence.
//! * If a hangul-syllable doesn't contain CHOSEONG or JUNGSEONG jamos,
//!   corresponding filter (U+115F, U+1160) will be automatically added.
//! * An orphan Regional Indicator (U+1F1E6..U+1F1FF) abstract character is
//!   automatically appended another copy to make it no longer orphan.
//! * An Extended Pictographic sequence that ends with the abstract character ZWJ (U+200D)
//!   with an optional sequence of continuing characters before it,
//!   will get another extra ZWJ (U+200D) abstract character to prevent it merging with next rune to
//!   form a larger Extended Pictographic sequence.
//! * If no base character is provided, a space (U+0020) character is inserted.

pub(crate) mod tables;

pub(crate) mod rune_ty;

pub(crate) mod rune_registry;

pub(crate) mod rune_str_ty;

pub(crate) mod rune_string;

pub(crate) mod fss_utf;

pub use rune_ty::rune;

pub use rune_str_ty::RuneStr;

pub use rune_string::RuneString;

/// An slice of `rune`s
pub type RuneSlice = [rune];

/// A `Vec` of `rune`s
pub type RuneVec = Vec<rune>;
