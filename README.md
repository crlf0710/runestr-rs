`rune`, `RuneStr` and `RuneString`
=============

User-perceived characters type `rune` and its related types and data structures.

<br>

## Example

```rust
use runestr::{rune, RuneString};

fn main() {
    let runestr = RuneString::from_str_lossy("\u{0041}\u{0341}\u{304B}\u{3099}\u{9508}");
    assert_eq!(3, runestr.runes().count());
}
```

<br>

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>