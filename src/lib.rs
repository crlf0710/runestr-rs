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

pub(crate) mod tables;

pub(crate) mod rune_ty;

pub(crate) mod rune_registry;

pub(crate) mod rune_str_ty;

pub(crate) mod rune_string;

pub(crate) mod fss_utf;

pub use rune_ty::rune;

pub use rune_str_ty::RuneStr;

pub use rune_string::RuneString;

/// A slice of `rune`s
pub type RuneSlice = [rune];

/// A `Vec` of `rune`s
pub type RuneVec = Vec<rune>;
