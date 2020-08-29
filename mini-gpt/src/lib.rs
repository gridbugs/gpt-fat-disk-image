mod crc32;
pub mod gpt_header;
pub mod mbr;

pub const LOGICAL_BLOCK_SIZE: usize = 512;

pub use gpt_header::GptHeader;
pub use mbr::Mbr;

fn nth_logical_block(raw: &[u8], n: usize) -> &[u8] {
    let base = n * LOGICAL_BLOCK_SIZE;
    &raw[base..(base + LOGICAL_BLOCK_SIZE)]
}

pub fn read_mbr(raw: &[u8]) -> Result<Mbr, mbr::InvalidSignature> {
    Mbr::from_logical_block(nth_logical_block(raw, 0))
}

pub fn read_gpt_header(raw: &[u8]) -> Result<GptHeader, gpt_header::ContentError> {
    GptHeader::from_logical_block(nth_logical_block(raw, 1))
}
