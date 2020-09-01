mod attributes {
    pub const READ_ONLY: u8 = 0x01;
    pub const HIDDEN: u8 = 0x02;
    pub const SYSTEM: u8 = 0x04;
    pub const VOLUME_ID: u8 = 0x08;
    pub const DIRECTORY: u8 = 0x10;

    pub const LONG_NAME: u8 = READ_ONLY | HIDDEN | SYSTEM | VOLUME_ID;
}

#[derive(Debug)]
struct RawDirectoryEntryNormal {
    short_filename_main: String,
    short_filename_extension: String,
    attributes: u8,
    first_cluster: u32,
    file_size: u32,
}

#[derive(Debug)]
struct RawDirectoryEntryLongName {
    order: u8,
    name: String,
    attributes: u8,
    checksum: u8,
}

#[derive(Debug)]
enum RawDirectoryEntry {
    Normal(RawDirectoryEntryNormal),
    LongName(RawDirectoryEntryLongName),
}

impl RawDirectoryEntry {
    fn parse(raw: &[u8]) -> Self {
        use std::convert::TryInto;
        let attributes = raw[11];
        if attributes == attributes::LONG_NAME {
            let order = raw[0];
            let name1 = &raw[1..11];
            let checksum = raw[13];
            let name2 = &raw[14..26];
            let name3 = &raw[28..32];
            let name = String::from_utf16_lossy(
                &name1
                    .chunks(2)
                    .chain(name2.chunks(2))
                    .chain(name3.chunks(2))
                    .map(|c| u16::from_le_bytes(c.try_into().unwrap()))
                    .take_while(|&c| c != 0)
                    .collect::<Vec<_>>(),
            );
            Self::LongName(RawDirectoryEntryLongName {
                order,
                name,
                attributes,
                checksum,
            })
        } else {
            fn slice_to_string(slice: &[u8]) -> String {
                const SKIP_SPACE: u8 = 0x20;
                String::from_utf8_lossy(
                    &slice
                        .into_iter()
                        .cloned()
                        .skip_while(|&c| c == SKIP_SPACE)
                        .take_while(|&c| c != SKIP_SPACE)
                        .collect::<Vec<_>>(),
                )
                .to_string()
            }
            let short_filename_main = slice_to_string(&raw[0..8]);
            let short_filename_extension = slice_to_string(&raw[8..11]);
            let first_cluster_hi = u16::from_le_bytes(raw[20..22].try_into().unwrap());
            let first_cluster_lo = u16::from_le_bytes(raw[26..28].try_into().unwrap());
            let first_cluster = ((first_cluster_hi as u32) << 16) | (first_cluster_lo as u32);
            let file_size = u32::from_le_bytes(raw[28..32].try_into().unwrap());
            Self::Normal(RawDirectoryEntryNormal {
                short_filename_main,
                short_filename_extension,
                attributes,
                first_cluster,
                file_size,
            })
        }
    }
}

#[derive(Debug)]
pub struct DirectoryEntry {
    pub short_name: String,
    pub long_name: Option<String>,
    pub file_size: u32,
    pub first_cluster: u32,
    pub attributes: u8,
}

impl DirectoryEntry {
    pub fn name(&self) -> &str {
        self.long_name
            .as_ref()
            .map_or_else(|| self.short_name.as_str(), |long_name| long_name.as_str())
    }
    pub fn is_directory(&self) -> bool {
        self.attributes & attributes::DIRECTORY != 0
    }
}

pub struct Directory {
    entries: Vec<DirectoryEntry>,
}

pub const DIRECTORY_ENTRY_BYTES: usize = 32;
const UNUSED_ENTRY_PREFIX: u8 = 0xE5;
const END_OF_DIRECTORY_PREFIX: u8 = 0;

impl Directory {
    fn raw_from_cluster_into_iter<'a, I>(iter: I) -> impl 'a + Iterator<Item = RawDirectoryEntry>
    where
        I: 'a + IntoIterator<Item = &'a [u8]>,
    {
        iter.into_iter().flat_map(|cluster| {
            cluster
                .chunks(DIRECTORY_ENTRY_BYTES)
                .take_while(|raw_entry| raw_entry[0] != END_OF_DIRECTORY_PREFIX)
                .filter(|raw_entry| raw_entry[0] != UNUSED_ENTRY_PREFIX)
                .map(RawDirectoryEntry::parse)
        })
    }

    fn raw_from_contiguous<'a>(raw: &'a [u8]) -> impl 'a + Iterator<Item = RawDirectoryEntry> {
        raw.chunks(DIRECTORY_ENTRY_BYTES)
            .take_while(|raw_entry| raw_entry[0] != END_OF_DIRECTORY_PREFIX)
            .filter(|raw_entry| raw_entry[0] != UNUSED_ENTRY_PREFIX)
            .map(RawDirectoryEntry::parse)
    }

    fn raw_entries_to_entries<I>(iter: I) -> impl Iterator<Item = DirectoryEntry>
    where
        I: IntoIterator<Item = RawDirectoryEntry>,
    {
        let mut name_parts = Vec::new();
        iter.into_iter()
            .filter_map(move |raw_entry| match raw_entry {
                RawDirectoryEntry::LongName(long_name_entry) => {
                    name_parts.push(long_name_entry.name);
                    None
                }
                RawDirectoryEntry::Normal(normal_entry) => {
                    let long_name = if name_parts.is_empty() {
                        None
                    } else {
                        let mut long_names = String::new();
                        long_names.extend(name_parts.drain(..).rev());
                        Some(long_names)
                    };
                    let short_name = if normal_entry.short_filename_extension.is_empty() {
                        normal_entry.short_filename_main
                    } else {
                        format!(
                            "{}.{}",
                            normal_entry.short_filename_main, normal_entry.short_filename_extension
                        )
                    };
                    Some(DirectoryEntry {
                        short_name,
                        long_name,
                        file_size: normal_entry.file_size,
                        first_cluster: normal_entry.first_cluster,
                        attributes: normal_entry.attributes,
                    })
                }
            })
    }

    pub(crate) fn from_cluster_into_iter<'a, I>(iter: I) -> Self
    where
        I: 'a + IntoIterator<Item = &'a [u8]>,
    {
        Self {
            entries: Self::raw_entries_to_entries(Self::raw_from_cluster_into_iter(iter)).collect(),
        }
    }

    pub(crate) fn from_contiguous(raw: &[u8]) -> Self {
        Self {
            entries: Self::raw_entries_to_entries(Self::raw_from_contiguous(raw)).collect(),
        }
    }

    pub fn entries(&self) -> &[DirectoryEntry] {
        &self.entries
    }
}
