use crate::gpt_header::GptHeader;
use crate::LOGICAL_BLOCK_SIZE;
use std::fmt;

#[derive(Debug)]
pub struct PartitionEntryArray {
    pub entries: Vec<PartitionEntry>,
}

#[derive(Debug)]
pub struct PartitionEntry {
    pub partition_type_guid: u128,
    pub unique_partition_guid: u128,
    pub starting_lba: u64,
    pub ending_lba: u64,
    pub attributes: u64,
    pub partition_name: String,
}

#[derive(Debug, Clone, Copy)]
pub enum PartitionEntryError {
    ChecksumMismatch { computed: u32, read: u32 },
}

impl PartitionEntryArray {
    pub fn new(raw: &[u8], header: &GptHeader) -> Result<Self, PartitionEntryError> {
        let partition_entry_array_start_index =
            header.partition_entry_lba as usize * LOGICAL_BLOCK_SIZE;
        let partition_entry_array_size =
            (header.size_of_partition_entry * header.number_of_partition_entries) as usize;
        let partition_entry_array_bytes = &raw[partition_entry_array_start_index
            ..(partition_entry_array_start_index + partition_entry_array_size)];
        let computed_crc32 = crate::crc32::crc32(partition_entry_array_bytes);
        if computed_crc32 != header.partition_entry_array_crc32 {
            return Err(PartitionEntryError::ChecksumMismatch {
                computed: computed_crc32,
                read: header.partition_entry_array_crc32,
            });
        }
        Ok(Self {
            entries: partition_entry_array_bytes
                .chunks(header.size_of_partition_entry as usize)
                .map(PartitionEntry::new)
                .collect(),
        })
    }
}

impl PartitionEntry {
    fn new(bytes: &[u8]) -> Self {
        use std::convert::TryInto;
        let partition_type_guid = u128::from_le_bytes(bytes[0..16].try_into().unwrap());
        let unique_partition_guid = u128::from_le_bytes(bytes[16..32].try_into().unwrap());
        let starting_lba = u64::from_le_bytes(bytes[32..40].try_into().unwrap());
        let ending_lba = u64::from_le_bytes(bytes[40..48].try_into().unwrap());
        let attributes = u64::from_le_bytes(bytes[48..56].try_into().unwrap());
        let partition_name_bytes = &bytes[56..128];
        let partition_name = String::from_utf16_lossy(
            &partition_name_bytes
                .chunks(2)
                .map(|c| u16::from_le_bytes(c.try_into().unwrap()))
                .take_while(|&c| c != 0)
                .collect::<Vec<_>>(),
        );
        Self {
            partition_type_guid,
            unique_partition_guid,
            starting_lba,
            ending_lba,
            attributes,
            partition_name,
        }
    }

    pub fn is_used(&self) -> bool {
        self.partition_type_guid != 0
    }
}

impl fmt::Display for PartitionEntryArray {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (i, entry) in self.entries.iter().enumerate() {
            if entry.partition_type_guid != 0 {
                write!(f, "{} => {:#X?}", i, entry)?;
            }
        }
        Ok(())
    }
}
