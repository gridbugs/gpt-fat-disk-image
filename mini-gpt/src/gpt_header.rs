use std::fmt;

#[derive(Debug, Clone, Copy)]
pub struct GptHeader {
    pub signature: u64,
    pub revision: u32,
    pub header_size: u32,
    pub header_crc32: u32,
    pub my_lba: u64,
    pub alternate_lba: u64,
    pub first_usable_lba: u64,
    pub last_usable_lba: u64,
    pub disk_guid: u128,
    pub partition_entry_lba: u64,
    pub number_of_partition_entries: u32,
    pub size_of_partition_entry: u32,
    pub partition_entry_array_crc32: u32,
}

use crate::LOGICAL_BLOCK_SIZE;
pub const REQUIRED_SIGNATURE: u64 = 0x5452415020494645;
pub const THIS_REVISION: u32 = 0x10000;
pub const MIN_HEADER_SIZE: u32 = 92;

#[derive(Debug, Clone, Copy)]
pub enum ContentError {
    InvalidSignature(u64),
    IncorrectRevision(u32),
    InvalidHeaderSize(u32),
    UnexpectedNonZeroValue,
    ChecksumMismatch { computed: u32, read: u32 },
}

impl GptHeader {
    pub fn from_logical_block(logical_block: &[u8]) -> Result<Self, ContentError> {
        use std::convert::TryInto;
        let signature = u64::from_le_bytes(logical_block[0..8].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(ContentError::InvalidSignature(signature));
        }
        let revision = u32::from_le_bytes(logical_block[8..12].try_into().unwrap());
        if revision != THIS_REVISION {
            return Err(ContentError::IncorrectRevision(revision));
        }
        let header_size = u32::from_le_bytes(logical_block[12..16].try_into().unwrap());
        if header_size < MIN_HEADER_SIZE || header_size as usize > LOGICAL_BLOCK_SIZE {
            return Err(ContentError::InvalidHeaderSize(header_size));
        }
        let header_crc32 = u32::from_le_bytes(logical_block[16..20].try_into().unwrap());
        let computed_crc32 = Self::crc32_from_logical_block(logical_block, header_size);
        if computed_crc32 != header_crc32 {
            return Err(ContentError::ChecksumMismatch {
                computed: computed_crc32,
                read: header_crc32,
            });
        }
        if u32::from_le_bytes(logical_block[20..24].try_into().unwrap()) != 0 {
            return Err(ContentError::UnexpectedNonZeroValue);
        }
        let my_lba = u64::from_le_bytes(logical_block[24..32].try_into().unwrap());
        let alternate_lba = u64::from_le_bytes(logical_block[32..40].try_into().unwrap());
        let first_usable_lba = u64::from_le_bytes(logical_block[40..48].try_into().unwrap());
        let last_usable_lba = u64::from_le_bytes(logical_block[48..56].try_into().unwrap());
        let disk_guid = u128::from_le_bytes(logical_block[56..72].try_into().unwrap());
        let partition_entry_lba = u64::from_le_bytes(logical_block[72..80].try_into().unwrap());
        let number_of_partition_entries =
            u32::from_le_bytes(logical_block[80..84].try_into().unwrap());
        let size_of_partition_entry = u32::from_le_bytes(logical_block[84..88].try_into().unwrap());
        let partition_entry_array_crc32 =
            u32::from_le_bytes(logical_block[88..92].try_into().unwrap());
        for &b in &logical_block[92..] {
            if b != 0 {
                return Err(ContentError::UnexpectedNonZeroValue);
            }
        }
        Ok(Self {
            signature,
            revision,
            header_size,
            header_crc32,
            my_lba,
            alternate_lba,
            first_usable_lba,
            last_usable_lba,
            disk_guid,
            partition_entry_lba,
            number_of_partition_entries,
            size_of_partition_entry,
            partition_entry_array_crc32,
        })
    }
    fn crc32_from_logical_block(logical_block: &[u8], header_size: u32) -> u32 {
        let mut copy = [0; LOGICAL_BLOCK_SIZE];
        copy.copy_from_slice(logical_block);
        // zero-out the crc field of the copy
        copy[16] = 0;
        copy[17] = 0;
        copy[18] = 0;
        copy[19] = 0;
        crate::crc32::crc32(&copy[0..(header_size as usize)])
    }
}

impl fmt::Display for GptHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "{:#X?}", self)?;
        Ok(())
    }
}
