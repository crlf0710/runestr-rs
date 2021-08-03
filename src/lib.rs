#![deny(unsafe_op_in_unsafe_fn)]
#![deny(warnings, missing_debug_implementations)]
#![allow(irrefutable_let_patterns, dead_code)]

pub(crate) mod tables;

pub(crate) mod rune_ty;

pub(crate) mod rune_registry;

pub(crate) mod rune_str_ty;

pub(crate) mod rune_string;

pub(crate) mod fss_utf;

pub use rune_ty::rune;

pub use rune_str_ty::RuneStr;

pub use rune_string::RuneString;
