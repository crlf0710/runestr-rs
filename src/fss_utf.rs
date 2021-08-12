use std::convert::TryInto;

pub(crate) const MAX_BYTE_COUNT: usize = 6;

const TAG_CONT: u8 = 0b1000_0000;
const TAG_TWO_B: u8 = 0b1100_0000;
const TAG_THREE_B: u8 = 0b1110_0000;
const TAG_FOUR_B: u8 = 0b1111_0000;
const TAG_FIVE_B: u8 = 0b1111_1000;
const TAG_SIX_B: u8 = 0b1111_1100;
const END_ONE_B: u32 = 0x80;
const END_TWO_B: u32 = 0x800;
const END_THREE_B: u32 = 0x10000;
const END_FOUR_B: u32 = 0x20_0000;
const END_FIVE_B: u32 = 0x400_0000;
const END_SIX_B: u32 = 0x8000_0000;

pub(crate) fn len(code: u32) -> usize {
    if code < END_ONE_B {
        1
    } else if code < END_TWO_B {
        2
    } else if code < END_THREE_B {
        3
    } else if code < END_FOUR_B {
        4
    } else if code < END_FIVE_B {
        5
    } else if code < END_SIX_B {
        6
    } else {
        unreachable!()
    }
}

pub(crate) fn len_from_first_byte(v: u8) -> usize {
    if v < 128 {
        1
    } else if v & TAG_SIX_B == TAG_SIX_B {
        6
    } else if v & TAG_FIVE_B == TAG_FIVE_B {
        5
    } else if v & TAG_FOUR_B == TAG_FOUR_B {
        4
    } else if v & TAG_THREE_B == TAG_THREE_B {
        3
    } else if v & TAG_TWO_B == TAG_TWO_B {
        2
    } else {
        unreachable!()
    }
}

pub(crate) fn cont_len_from_first_byte(v: u8) -> usize {
    if v < 128 {
        0
    } else if v & TAG_SIX_B == TAG_SIX_B {
        5
    } else if v & TAG_FIVE_B == TAG_FIVE_B {
        4
    } else if v & TAG_FOUR_B == TAG_FOUR_B {
        3
    } else if v & TAG_THREE_B == TAG_THREE_B {
        2
    } else if v & TAG_TWO_B == TAG_TWO_B {
        1
    } else {
        unreachable!()
    }
}

pub(crate) fn try_cont_len_from_first_byte(v: u8) -> Option<usize> {
    if v < 128 {
        Some(0)
    } else if v & TAG_SIX_B == TAG_SIX_B {
        Some(5)
    } else if v & TAG_FIVE_B == TAG_FIVE_B {
        Some(4)
    } else if v & TAG_FOUR_B == TAG_FOUR_B {
        Some(3)
    } else if v & TAG_THREE_B == TAG_THREE_B {
        Some(2)
    } else if v & TAG_TWO_B == TAG_TWO_B {
        Some(1)
    } else {
        None
    }
}

const CONT_PREFIX_MASK: u8 = 0b1100_0000;
const CONT_VALUE_MASK: u8 = 0b0011_1111;

pub(crate) fn is_cont_byte(v: u8) -> bool {
    (v & CONT_PREFIX_MASK) == TAG_CONT
}

pub(crate) fn encode_fss_utf(
    code: u32,
    buf: &mut [u8],
) -> Result<usize, std::array::TryFromSliceError> {
    let bytes: &mut [u8; 6] = buf.try_into()?;
    let len = len(code);
    match (len, &mut bytes[..]) {
        (1, [b0, ..]) => {
            *b0 = code as u8;
        }
        (2, [b0, b1, ..]) => {
            *b0 = (code >> 6 & 0x1F) as u8 | TAG_TWO_B;
            *b1 = (code & 0x3F) as u8 | TAG_CONT;
        }
        (3, [b0, b1, b2, ..]) => {
            *b0 = (code >> 12 & 0x0F) as u8 | TAG_THREE_B;
            *b1 = (code >> 6 & 0x3F) as u8 | TAG_CONT;
            *b2 = (code & 0x3F) as u8 | TAG_CONT;
        }
        (4, [b0, b1, b2, b3, ..]) => {
            *b0 = (code >> 18 & 0x07) as u8 | TAG_FOUR_B;
            *b1 = (code >> 12 & 0x3F) as u8 | TAG_CONT;
            *b2 = (code >> 6 & 0x3F) as u8 | TAG_CONT;
            *b3 = (code & 0x3F) as u8 | TAG_CONT;
        }
        (5, [b0, b1, b2, b3, b4, ..]) => {
            *b0 = (code >> 24 & 0x03) as u8 | TAG_FIVE_B;
            *b1 = (code >> 18 & 0x3F) as u8 | TAG_CONT;
            *b2 = (code >> 12 & 0x3F) as u8 | TAG_CONT;
            *b3 = (code >> 6 & 0x3F) as u8 | TAG_CONT;
            *b4 = (code & 0x3F) as u8 | TAG_CONT;
        }
        (6, [b0, b1, b2, b3, b4, b5, ..]) => {
            *b0 = (code >> 30 & 0x01) as u8 | TAG_SIX_B;
            *b1 = (code >> 24 & 0x3F) as u8 | TAG_CONT;
            *b2 = (code >> 18 & 0x3F) as u8 | TAG_CONT;
            *b3 = (code >> 12 & 0x3F) as u8 | TAG_CONT;
            *b4 = (code >> 6 & 0x3F) as u8 | TAG_CONT;
            *b5 = (code & 0x3F) as u8 | TAG_CONT;
        }
        _ => unreachable!(),
    };
    Ok(len)
}

#[inline]
fn fss_utf_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

#[inline]
fn fss_utf_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_VALUE_MASK) as u32
}

#[inline]
pub(crate) fn decode_fss_utf_value(h: u8, cont: &[u8]) -> u32 {
    let mut v = fss_utf_first_byte(h, cont.len() as u32);
    for c in cont.iter().cloned() {
        v = fss_utf_acc_cont_byte(v, c);
    }
    v
}

#[inline]
pub(crate) fn try_decode_fss_utf_value(h: u8, cont: &[u8]) -> Option<u32> {
    let mut v = fss_utf_first_byte(h, cont.len() as u32);
    for c in cont.iter().cloned() {
        if !is_cont_byte(c) {
            return None;
        }
        v = fss_utf_acc_cont_byte(v, c);
    }
    Some(v)
}
