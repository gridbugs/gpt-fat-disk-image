pub mod mbr;

pub const LOGICAL_BLOCK_SIZE: usize = 512;

pub use mbr::Mbr;

pub fn read_mbr(raw: &[u8]) -> Result<Mbr, mbr::InvalidSignature> {
    Mbr::from_logical_block(&raw[0..LOGICAL_BLOCK_SIZE])
}
