use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct GptHeader {
    pub signature: u64,
    pub revision: u32,
    pub header_size: u32,
    pub header_crc32: u32,
}

use crate::LOGICAL_BLOCK_SIZE;
pub const REQUIRED_SIGNATURE: u64 = 0x5452415020494645;
pub const THIS_REVISION: u32 = 0x10000;
pub const MIN_HEADER_SIZE: u32 = 92;

#[derive(Debug, Clone, Copy)]
pub enum Error {
    InvalidSignature(u64),
    IncorrectRevision(u32),
    InvalidHeaderSize(u32),
}

impl GptHeader {
    pub fn from_logical_block(logical_block: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let signature = u64::from_le_bytes(logical_block[0..8].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(Error::InvalidSignature(signature));
        }
        let revision = u32::from_le_bytes(logical_block[8..12].try_into().unwrap());
        if revision != THIS_REVISION {
            return Err(Error::IncorrectRevision(revision));
        }
        let header_size = u32::from_le_bytes(logical_block[12..16].try_into().unwrap());
        if header_size < MIN_HEADER_SIZE || header_size as usize > LOGICAL_BLOCK_SIZE {
            return Err(Error::InvalidHeaderSize(header_size));
        }
        let header_crc32 = u32::from_le_bytes(logical_block[16..20].try_into().unwrap());

        Ok(Self {
            signature,
            revision,
            header_size,
            header_crc32,
        })
    }
}

impl fmt::Display for GptHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "{:#X?}", self)?;
        Ok(())
    }
}
