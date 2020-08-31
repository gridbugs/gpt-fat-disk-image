use std::fmt;

mod crc32;
mod gpt_header;
mod mbr;
mod partition_entry_array;

pub use gpt_header::ContentError;
pub use mbr::InvalidSignature;
pub use partition_entry_array::{PartitionEntry, PartitionEntryError};

const LOGICAL_BLOCK_SIZE: usize = 512;

use gpt_header::GptHeader;
use mbr::Mbr;
use partition_entry_array::PartitionEntryArray;

fn nth_logical_block_raw(raw: &[u8], n: usize) -> &[u8] {
    let base = n * LOGICAL_BLOCK_SIZE;
    &raw[base..(base + LOGICAL_BLOCK_SIZE)]
}

#[derive(Debug)]
pub struct GptMetadata<'a> {
    mbr: Mbr<'a>,
    header: GptHeader,
    partition_entry_array: PartitionEntryArray,
}

#[derive(Debug, Clone, Copy)]
pub enum GptMetadataError {
    MbrError(mbr::InvalidSignature),
    HeaderContentError(gpt_header::ContentError),
    BackupHeaderContentError(gpt_header::ContentError),
    MyLbaInconsintent,
    AlternativeLbaNotLastBlock,
    BackupMyLbaInconsistent,
    BackupAlternativeLbaNotBlock1,
    DataSizeNotMultipleOfBlockSize,
    PartitionEntryError(partition_entry_array::PartitionEntryError),
}

impl<'a> GptMetadata<'a> {
    fn parse(raw: &'a [u8]) -> Result<Self, GptMetadataError> {
        let num_blocks = raw.len() / LOGICAL_BLOCK_SIZE;
        if num_blocks * LOGICAL_BLOCK_SIZE != raw.len() {
            return Err(GptMetadataError::DataSizeNotMultipleOfBlockSize);
        }
        let mbr = Mbr::from_logical_block(nth_logical_block_raw(raw, 0))
            .map_err(GptMetadataError::MbrError)?;
        let header = GptHeader::from_logical_block(nth_logical_block_raw(raw, 1))
            .map_err(GptMetadataError::HeaderContentError)?;
        if header.my_lba != 1 {
            return Err(GptMetadataError::MyLbaInconsintent);
        }
        if header.alternate_lba != (num_blocks - 1) as u64 {
            return Err(GptMetadataError::AlternativeLbaNotLastBlock);
        }
        // For now, just check that the backup header exists, but don't attempt to fall back on it
        // in the event that the primary header is corrupt.
        let backup_header =
            GptHeader::from_logical_block(nth_logical_block_raw(raw, num_blocks - 1))
                .map_err(GptMetadataError::BackupHeaderContentError)?;
        if backup_header.my_lba != (num_blocks - 1) as u64 {
            return Err(GptMetadataError::BackupMyLbaInconsistent);
        }
        if backup_header.alternate_lba != 1 {
            return Err(GptMetadataError::BackupAlternativeLbaNotBlock1);
        }
        let partition_entry_array = PartitionEntryArray::new(raw, &header)
            .map_err(GptMetadataError::PartitionEntryError)?;
        let _backup_partition_entry_array = PartitionEntryArray::new(raw, &backup_header)
            .map_err(GptMetadataError::PartitionEntryError)?;
        Ok(Self {
            mbr,
            header,
            partition_entry_array,
        })
    }

    pub fn partition_entries(&self) -> &[PartitionEntry] {
        &self.partition_entry_array.entries
    }
}

pub struct GptReader<'a> {
    raw: &'a [u8],
    metadata: GptMetadata<'a>,
}

impl<'a> GptReader<'a> {
    pub fn new(raw: &'a [u8]) -> Result<Self, GptMetadataError> {
        Ok(Self {
            raw,
            metadata: GptMetadata::parse(raw)?,
        })
    }

    pub fn metadata(&self) -> &GptMetadata {
        &self.metadata
    }

    pub fn nth_partition_data(&self, n: usize) -> Option<PartitionData> {
        self.metadata
            .partition_entry_array
            .entries
            .get(n)
            .map(|partition_entry| {
                let start_index = partition_entry.starting_lba as usize * LOGICAL_BLOCK_SIZE;
                let end_index = (partition_entry.ending_lba as usize + 1) * LOGICAL_BLOCK_SIZE;
                PartitionData {
                    raw: &self.raw[start_index..end_index],
                }
            })
    }
}

impl<'a> fmt::Display for GptMetadata<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "MBR:\n{}\n", &self.mbr)?;
        writeln!(f, "Header:\n{}\n", &self.header)?;
        writeln!(
            f,
            "Partition Entry Array:\n{}\n",
            &self.partition_entry_array
        )?;
        Ok(())
    }
}

pub struct PartitionData<'a> {
    pub raw: &'a [u8],
}

impl<'a> fmt::Display for PartitionData<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        mini_hex_dump::display_bytes(self.raw, 16, f)?;
        Ok(())
    }
}
