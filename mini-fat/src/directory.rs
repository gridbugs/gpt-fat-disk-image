#[derive(Debug)]
pub struct DirectoryEntry {
    pub short_filename: String,
    pub attributes: u8,
    pub first_cluster: u32,
    pub file_size: u32,
}

impl DirectoryEntry {
    pub fn new(raw: &[u8]) -> Self {
        use std::convert::TryInto;
        let short_filename = String::from_utf8_lossy(&raw[0..11]).to_string();
        let attributes = raw[11];
        let first_cluster_hi = u16::from_le_bytes(raw[20..22].try_into().unwrap());
        let first_cluster_lo = u16::from_le_bytes(raw[26..28].try_into().unwrap());
        let first_cluster = ((first_cluster_hi as u32) << 16) | (first_cluster_lo as u32);
        let file_size = u32::from_le_bytes(raw[28..32].try_into().unwrap());
        Self {
            short_filename,
            attributes,
            first_cluster,
            file_size,
        }
    }
}

pub struct Directory {
    entries: Vec<DirectoryEntry>,
}

pub const DIRECTORY_ENTRY_BYTES: usize = 32;
const UNUSED_ENTRY_PREFIX: u8 = 0xE5;
const END_OF_DIRECTORY_PREFIX: u8 = 0;

impl Directory {
    pub(crate) fn from_contiguous(raw: &[u8]) -> Self {
        Self {
            entries: raw
                .chunks(DIRECTORY_ENTRY_BYTES)
                .take_while(|raw_entry| raw_entry[0] != END_OF_DIRECTORY_PREFIX)
                .filter(|raw_entry| raw_entry[0] != UNUSED_ENTRY_PREFIX)
                .map(DirectoryEntry::new)
                .collect(),
        }
    }

    pub fn entries(&self) -> &[DirectoryEntry] {
        &self.entries
    }
}
