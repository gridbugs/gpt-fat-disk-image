pub use anyhow::Error;
use std::fmt;
use std::fs::File;
use std::io;
use std::ops::Range;
use std::path;

mod create {
    pub const BYTES_PER_SECTOR: u16 = 512;
    pub const SECTORS_PER_CLUSTER: u8 = 1;
    pub const BYTES_PER_CLUSTER: u32 = BYTES_PER_SECTOR as u32 * SECTORS_PER_CLUSTER as u32;
    pub const FAT_ENTRY_SIZE_BYTES: u8 = 4; // all created images will be formatted as FAT32
    pub const MIN_NUM_CLUSTERS: u64 = 65526; // 1 larger than the technical minimum for FAT32 to mitigate off-by-1 errors in firmware
    pub const NUM_FATS: u8 = 2;
    pub const RESERVED_SECTOR_COUNT: u16 = 32;
    pub const JMP_BOOT: [u8; 3] = [235, 88, 144]; // copied from mkfs.fat
    pub const OEM_NAME: &str = "mini_fat";
    pub const MEDIA_FIXED: u8 = 0xF8;
    pub const SECTORS_PER_TRACK: u16 = 32; // copied from mkfs.fat
    pub const NUM_HEADS: u16 = 64; // copied from mkfs.fat
    pub const NUM_HIDDEN_SECTORS: u32 = 0;
    pub const VERSION: u16 = 0;
    pub const ROOT_CLUSTER: u32 = 2;
    pub const FS_INFO: u16 = 1;
    pub const BK_BOOT_SECTOR: u16 = 6;
    pub const DRIVE_NUM: u8 = 128; // copied from mkfs.fat
    pub const VOLUME_LABEL: &str = "NO NAME    "; // copied from mkfs.fat
    pub const FAT32_FILE_SYSTEM_TYPE: &str = "FAT32   "; // copied from mkfs.fat
}

#[derive(Debug)]
pub enum FatError {
    UnexpectedNonZero {
        byte_index: usize,
    },
    ExactlyOneTotalSectorsFieldMustBeZero {
        total_sectors_16: u16,
        total_sectors_32: u32,
    },
    ExactlyOneFatSizeMustBeZero {
        fat_size_16: u16,
        fat_size_32: u32,
    },
    InvalidSignature(u16),
    InvalidFatEntry(u32),
    FatLookup(FatLookupError),
    NoSuchFile,
    InvalidPath,
    InvalidDiskPath(String),
    ExpectedFileFoundDirectory,
    BpbDoesNotMatchBackupBpb,
    InvalidFsInfoLeadSignature(u32),
    InvalidFsInfoStrucSignature(u32),
    InvalidFsInfoTrailSignature(u32),
}

impl fmt::Display for FatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for FatError {}

#[derive(Debug, PartialEq, Eq)]
struct Bpb {
    jmp_boot: [u8; 3],
    oem_name: String,
    bytes_per_sector: u16,
    sectors_per_cluster: u8,
    reserved_sector_count: u16,
    num_fats: u8,
    root_entry_count: u16,
    total_sectors_16: u16,
    media: u8,
    fat_size_16: u16,
    sectors_per_track: u16,
    num_heads: u16,
    hidden_sectors: u32,
    total_sectors_32: u32,
    fat_size_32: u32,
    ext_flags: u16,
    fs_version: u16,
    root_cluster: u32,
    fs_info: u16,
    bk_boot_sector: u16,
    drive_number: u8,
    boot_signature: u8,
    volume_id: u32,
    volume_label: String,
    file_system_type: String,
    signature: u16,
}

const BPB_SIZE: usize = 512;
const REQUIRED_SIGNATURE: u16 = 0xAA55;
const BOOT_SIGNATURE: u8 = 0x29;

impl Bpb {
    fn read<H>(handle: &mut H, partition_byte_start: u64, buf: &mut Vec<u8>) -> Result<Self, Error>
    where
        H: io::Seek + io::Read,
    {
        handle_read(handle, partition_byte_start, BPB_SIZE, buf)?;
        let bpb = Bpb::parse(&buf)?;
        if bpb.bk_boot_sector != 0 {
            handle_read(
                handle,
                partition_byte_start + (bpb.bytes_per_sector * bpb.bk_boot_sector) as u64,
                BPB_SIZE,
                buf,
            )?;
            let backup_bpb = Bpb::parse(&buf)?;
            if backup_bpb != bpb {
                return Err(FatError::BpbDoesNotMatchBackupBpb.into());
            }
        }
        Ok(bpb)
    }

    fn new_fat32_raw(
        hierarchy: &directory_hierarchy::DirectoryHierarchy,
    ) -> Result<[u8; BPB_SIZE], Error> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let mut raw = [0; BPB_SIZE];
        raw[0..3].copy_from_slice(&create::JMP_BOOT);
        let oem_bytes = create::OEM_NAME.as_bytes();
        assert!(oem_bytes.len() <= 8);
        raw[3..(3 + oem_bytes.len())].copy_from_slice(oem_bytes);
        raw[11..13].copy_from_slice(&create::BYTES_PER_SECTOR.to_le_bytes());
        raw[13] = create::SECTORS_PER_CLUSTER;
        raw[14..16].copy_from_slice(&create::RESERVED_SECTOR_COUNT.to_le_bytes());
        raw[16] = create::NUM_FATS;
        raw[17..19].copy_from_slice(&0u16.to_le_bytes()); // root entry count - 0 on fat32
        raw[19..21].copy_from_slice(&0u16.to_le_bytes()); // total sectors 16 - 0 on fat32
        raw[21] = create::MEDIA_FIXED;
        raw[22..24].copy_from_slice(&0u16.to_le_bytes()); // fat size 16 - 0 on fat32
        raw[24..26].copy_from_slice(&create::SECTORS_PER_TRACK.to_le_bytes());
        raw[26..28].copy_from_slice(&create::NUM_HEADS.to_le_bytes());
        raw[28..32].copy_from_slice(&create::NUM_HIDDEN_SECTORS.to_le_bytes());
        let num_sectors = hierarchy.implied_total_num_clusters()? as u32; // number of sectors in all regions of the partition
        raw[32..36].copy_from_slice(&num_sectors.to_le_bytes());
        let fat_size_in_bytes: u32 =
            hierarchy.implied_num_data_clusters()? as u32 * create::FAT_ENTRY_SIZE_BYTES as u32;
        let fat_size_in_sectors: u32 =
            ((fat_size_in_bytes - 1) / create::BYTES_PER_SECTOR as u32) + 1; // divide rounding up
        raw[36..40].copy_from_slice(&fat_size_in_sectors.to_le_bytes());
        raw[40..42].copy_from_slice(&0u16.to_le_bytes()); // flags
        raw[42..44].copy_from_slice(&create::VERSION.to_le_bytes());
        raw[44..48].copy_from_slice(&create::ROOT_CLUSTER.to_le_bytes());
        raw[48..50].copy_from_slice(&create::FS_INFO.to_le_bytes());
        raw[50..52].copy_from_slice(&create::BK_BOOT_SECTOR.to_le_bytes());
        raw[64] = create::DRIVE_NUM;
        raw[66] = BOOT_SIGNATURE;
        let since_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_else(|e| e.duration())
            .as_millis() as u32;
        raw[67..71].copy_from_slice(&since_epoch.to_le_bytes());
        let volume_label_bytes = create::VOLUME_LABEL.as_bytes();
        assert!(volume_label_bytes.len() <= 11);
        raw[71..(71 + volume_label_bytes.len())].copy_from_slice(volume_label_bytes);
        let file_system_type = create::FAT32_FILE_SYSTEM_TYPE.as_bytes();
        raw[82..(82 + file_system_type.len())].copy_from_slice(file_system_type);
        raw[510..512].copy_from_slice(&REQUIRED_SIGNATURE.to_le_bytes());
        Ok(raw)
    }

    fn parse(raw: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let jmp_boot = [raw[0], raw[1], raw[2]];
        let oem_name = String::from_utf8_lossy(&raw[3..11]).to_string();
        let bytes_per_sector = u16::from_le_bytes(raw[11..13].try_into().unwrap());
        let sectors_per_cluster = raw[13];
        let reserved_sector_count = u16::from_le_bytes(raw[14..16].try_into().unwrap());
        let num_fats = raw[16];
        let root_entry_count = u16::from_le_bytes(raw[17..19].try_into().unwrap());
        let total_sectors_16 = u16::from_le_bytes(raw[19..21].try_into().unwrap());
        let media = raw[21];
        let fat_size_16 = u16::from_le_bytes(raw[22..24].try_into().unwrap());
        let sectors_per_track = u16::from_le_bytes(raw[24..26].try_into().unwrap());
        let num_heads = u16::from_le_bytes(raw[26..28].try_into().unwrap());
        let hidden_sectors = u32::from_le_bytes(raw[28..32].try_into().unwrap());
        let total_sectors_32 = u32::from_le_bytes(raw[32..36].try_into().unwrap());
        let (
            fat_size_32,
            ext_flags,
            fs_version,
            root_cluster,
            fs_info,
            bk_boot_sector,
            drive_number,
            boot_signature,
            volume_id,
            volume_label,
            file_system_type,
        );
        if (total_sectors_16 == 0) && (total_sectors_32 != 0) {
            // FAT32
            fat_size_32 = u32::from_le_bytes(raw[36..40].try_into().unwrap());
            ext_flags = u16::from_le_bytes(raw[40..42].try_into().unwrap());
            fs_version = u16::from_le_bytes(raw[42..44].try_into().unwrap());
            root_cluster = u32::from_le_bytes(raw[44..48].try_into().unwrap());
            fs_info = u16::from_le_bytes(raw[48..50].try_into().unwrap());
            bk_boot_sector = u16::from_le_bytes(raw[50..52].try_into().unwrap());
            for i in 52..64 {
                if raw[i] != 0 {
                    return Err(FatError::UnexpectedNonZero { byte_index: i }.into());
                }
            }
            drive_number = raw[64];
            if raw[65] != 0 {
                // On some partitions in the wild this is non-zero
                log::warn!("Unexpected non-zero BPB field at byte offset 65");
            }
            boot_signature = raw[66];
            volume_id = u32::from_le_bytes(raw[67..71].try_into().unwrap());
            volume_label = String::from_utf8_lossy(&raw[71..82]).to_string();
            file_system_type = String::from_utf8_lossy(&raw[82..90]).to_string();
        } else if (total_sectors_16 != 0) && (total_sectors_32 == 0) {
            // FAT12 or FAT16
            fat_size_32 = 0;
            ext_flags = 0;
            fs_version = 0;
            root_cluster = 0;
            fs_info = 0;
            bk_boot_sector = 0;
            drive_number = raw[36];
            if raw[37] != 0 {
                return Err(FatError::UnexpectedNonZero { byte_index: 37 }.into());
            }
            boot_signature = raw[38];
            volume_id = u32::from_le_bytes(raw[39..43].try_into().unwrap());
            volume_label = String::from_utf8_lossy(&raw[43..54]).to_string();
            file_system_type = String::from_utf8_lossy(&raw[54..62]).to_string();
        } else {
            return Err(FatError::ExactlyOneTotalSectorsFieldMustBeZero {
                total_sectors_16,
                total_sectors_32,
            }
            .into());
        }
        if (fat_size_16 == 0) == (fat_size_32 == 0) {
            return Err(FatError::ExactlyOneFatSizeMustBeZero {
                fat_size_16,
                fat_size_32,
            }
            .into());
        }
        let signature = u16::from_le_bytes(raw[510..512].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(FatError::InvalidSignature(signature).into());
        }
        Ok(Self {
            jmp_boot,
            oem_name,
            bytes_per_sector,
            sectors_per_cluster,
            reserved_sector_count,
            num_fats,
            root_entry_count,
            total_sectors_16,
            media,
            fat_size_16,
            sectors_per_track,
            num_heads,
            hidden_sectors,
            total_sectors_32,
            fat_size_32,
            ext_flags,
            fs_version,
            root_cluster,
            fs_info,
            bk_boot_sector,
            drive_number,
            boot_signature,
            volume_id,
            volume_label,
            file_system_type,
            signature,
        })
    }

    fn fat_size_in_sectors(&self) -> u32 {
        if self.fat_size_16 != 0 && self.fat_size_32 == 0 {
            self.fat_size_16 as u32
        } else {
            debug_assert!(self.fat_size_16 == 0 && self.fat_size_32 != 0);
            self.fat_size_32
        }
    }

    fn count_of_clusters(&self) -> u32 {
        let root_dir_sectors = ((self.root_entry_count as u32 * 32)
            + (self.bytes_per_sector as u32 - 1))
            / self.bytes_per_sector as u32;
        let total_sectors = if self.total_sectors_16 != 0 {
            self.total_sectors_16 as u32
        } else {
            self.total_sectors_32
        };
        let data_sectors = total_sectors
            - (self.reserved_sector_count as u32
                + (self.num_fats as u32 * self.fat_size_in_sectors())
                + root_dir_sectors);
        data_sectors / self.sectors_per_cluster as u32
    }

    fn fat_type(&self) -> FatType {
        let count_of_clusters = self.count_of_clusters();
        if count_of_clusters < 4085 {
            FatType::Fat12
        } else if count_of_clusters < 65525 {
            FatType::Fat16
        } else {
            FatType::Fat32
        }
    }

    fn maximum_valid_cluster(&self) -> u32 {
        self.count_of_clusters() + 1
    }

    fn root_directory_size(&self) -> usize {
        debug_assert!((self.fat_type() == FatType::Fat32) == (self.root_entry_count == 0));
        self.root_entry_count as usize * DIRECTORY_ENTRY_BYTES
    }

    fn root_directory_offset(&self) -> u64 {
        (self.reserved_sector_count as u64 + (self.num_fats as u64 * self.fat_size_16 as u64))
            * self.bytes_per_sector as u64
    }

    fn fat_offset(&self) -> u64 {
        self.reserved_sector_count as u64 * self.bytes_per_sector as u64
    }

    fn data_offset(&self) -> u64 {
        self.root_directory_size() as u64
            + ((self.reserved_sector_count as u64
                + self.fat_size_in_sectors() as u64 * self.num_fats as u64)
                * self.bytes_per_sector as u64)
    }

    pub fn bytes_per_cluster(&self) -> u32 {
        self.bytes_per_sector as u32 * self.sectors_per_cluster as u32
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FatType {
    Fat12,
    Fat16,
    Fat32,
}

impl FatType {
    fn fat_entry_defective(self) -> u32 {
        match self {
            Self::Fat12 => 0xFF7,
            Self::Fat16 => 0xFFF7,
            Self::Fat32 => 0x0FFFFFF7,
        }
    }
}

mod directory_attributes {
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
        if attributes == directory_attributes::LONG_NAME {
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

#[derive(Debug, Clone)]
pub struct DirectoryEntry {
    short_name: String,
    long_name: Option<String>,
    file_size: u32,
    first_cluster: u32,
    attributes: u8,
}

impl DirectoryEntry {
    pub fn name(&self) -> &str {
        self.long_name
            .as_ref()
            .map_or_else(|| self.short_name.as_str(), |long_name| long_name.as_str())
    }
    pub fn is_directory(&self) -> bool {
        self.attributes & directory_attributes::DIRECTORY != 0
    }
}

pub struct Directory {
    entries: Vec<DirectoryEntry>,
}

const DIRECTORY_ENTRY_BYTES: usize = 32;
const UNUSED_ENTRY_PREFIX: u8 = 0xE5;
const END_OF_DIRECTORY_PREFIX: u8 = 0;

impl Directory {
    fn from_traverser<H>(traverser: &mut Traverser<H>, cluster_index: u32) -> Result<Self, Error>
    where
        H: io::Seek + io::Read,
    {
        let mut entries = Vec::new();
        traverser
            .traverse(cluster_index)
            .for_each::<_, ()>(|cluster_data| {
                Self::raw_entries_to_entries(Self::raw_from_contiguous(cluster_data)).for_each(
                    |entry| {
                        entries.push(entry);
                    },
                );
                None
            })?;
        Ok(Self { entries })
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

    fn from_contiguous(raw: &[u8]) -> Self {
        Self {
            entries: Self::raw_entries_to_entries(Self::raw_from_contiguous(raw)).collect(),
        }
    }

    pub fn entries(&self) -> &[DirectoryEntry] {
        &self.entries
    }

    pub fn find_entry(&self, name: &str) -> Option<&DirectoryEntry> {
        self.entries.iter().find(|entry| entry.name() == name)
    }
}

const FS_INFO_SIZE: usize = 512;
const FS_INFO_REQUIRED_LEAD_SIGNATURE: u32 = 0x41615252;
const FS_INFO_REQUIRED_STRUC_SIGNATURE: u32 = 0x61417272;
const FS_INFO_REQUIRED_TRAIL_SIGNATURE: u32 = 0xAA550000;

#[derive(Debug, PartialEq, Eq)]
struct FsInfo {
    lead_signature: u32,
    struc_signature: u32,
    free_count: u32,
    next_free: u32,
    trail_signature: u32,
}

#[derive(Debug)]
enum FsInfoWarning {
    BackupDoesNotMatch(Box<FsInfo>),
    NoBackup,
}

impl FsInfo {
    fn new_raw(free_count: u32, next_free: u32) -> [u8; FS_INFO_SIZE] {
        let mut raw = [0; FS_INFO_SIZE];
        raw[0..4].copy_from_slice(&FS_INFO_REQUIRED_LEAD_SIGNATURE.to_le_bytes());
        raw[484..488].copy_from_slice(&FS_INFO_REQUIRED_STRUC_SIGNATURE.to_le_bytes());
        raw[488..492].copy_from_slice(&free_count.to_le_bytes());
        raw[492..496].copy_from_slice(&next_free.to_le_bytes());
        raw[508..512].copy_from_slice(&FS_INFO_REQUIRED_TRAIL_SIGNATURE.to_le_bytes());
        raw
    }

    fn read<H>(
        bpb: &Bpb,
        handle: &mut H,
        partition_byte_start: u64,
        buf: &mut Vec<u8>,
    ) -> Result<(Self, Option<FsInfoWarning>), Error>
    where
        H: io::Seek + io::Read,
    {
        let fs_info_sector = bpb.fs_info as u64;
        handle_read(
            handle,
            partition_byte_start + (fs_info_sector * bpb.bytes_per_sector as u64),
            FS_INFO_SIZE,
            buf,
        )?;
        let fs_info = Self::parse(buf)?;
        let fs_info_backup_sector = (bpb.bk_boot_sector + bpb.fs_info) as u64;
        handle_read(
            handle,
            partition_byte_start + (fs_info_backup_sector * bpb.bytes_per_sector as u64),
            FS_INFO_SIZE,
            buf,
        )?;
        let warning = match Self::parse(buf) {
            Ok(fs_info_backup) => {
                if fs_info_backup != fs_info {
                    Some(FsInfoWarning::BackupDoesNotMatch(Box::new(fs_info_backup)))
                } else {
                    None
                }
            }
            Err(e) => match e.downcast_ref::<FatError>().unwrap() {
                FatError::InvalidFsInfoLeadSignature(_) => Some(FsInfoWarning::NoBackup),
                _other => return Err(e),
            },
        };
        Ok((fs_info, warning))
    }

    fn parse(raw: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let lead_signature = u32::from_le_bytes(raw[0..4].try_into().unwrap());
        if lead_signature != FS_INFO_REQUIRED_LEAD_SIGNATURE {
            return Err(FatError::InvalidFsInfoLeadSignature(lead_signature).into());
        }
        let struc_signature = u32::from_le_bytes(raw[484..488].try_into().unwrap());
        if struc_signature != FS_INFO_REQUIRED_STRUC_SIGNATURE {
            return Err(FatError::InvalidFsInfoStrucSignature(struc_signature).into());
        }
        let free_count = u32::from_le_bytes(raw[488..492].try_into().unwrap());
        let next_free = u32::from_le_bytes(raw[492..496].try_into().unwrap());
        let trail_signature = u32::from_le_bytes(raw[508..512].try_into().unwrap());
        if trail_signature != FS_INFO_REQUIRED_TRAIL_SIGNATURE {
            return Err(FatError::InvalidFsInfoTrailSignature(trail_signature).into());
        }
        Ok(Self {
            lead_signature,
            struc_signature,
            free_count,
            next_free,
            trail_signature,
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum FatLookupError {
    FreeCluster,
    DefectiveCluster,
    UnspecifiedEntryOne,
    ReservedEntry,
}

enum FileFatEntry {
    AllocatedCluster(u32),
    EndOfFile,
}

fn classify_fat_entry(
    fat_type: FatType,
    entry: u32,
    maximum_valid_cluster: u32,
) -> Result<FileFatEntry, FatLookupError> {
    match entry {
        0 => Err(FatLookupError::FreeCluster),
        1 => Err(FatLookupError::UnspecifiedEntryOne),
        entry => {
            if entry <= maximum_valid_cluster {
                Ok(FileFatEntry::AllocatedCluster(entry))
            } else if entry < fat_type.fat_entry_defective() {
                Err(FatLookupError::ReservedEntry)
            } else if entry == fat_type.fat_entry_defective() {
                Err(FatLookupError::DefectiveCluster)
            } else {
                Ok(FileFatEntry::EndOfFile)
            }
        }
    }
}

fn handle_read<H>(handle: &mut H, offset: u64, size: usize, buf: &mut Vec<u8>) -> Result<(), Error>
where
    H: io::Seek + io::Read,
{
    buf.resize(size, 0);
    handle.seek(io::SeekFrom::Start(offset))?;
    handle.read_exact(buf)?;
    Ok(())
}

fn fat_entry_of_nth_cluster<H>(
    handle: &mut H,
    fat_type: FatType,
    fat_start: u64,
    n: u32,
) -> Result<u32, Error>
where
    H: io::Seek + io::Read,
{
    // The first 2 entries of the FAT are unused
    debug_assert!(n >= 2);
    match fat_type {
        FatType::Fat32 => {
            let base = n as u64 * 4;
            handle.seek(io::SeekFrom::Start(fat_start + base))?;
            let mut buf = [0; 4];
            handle.read_exact(&mut buf)?;
            Ok(u32::from_le_bytes(buf) & 0x0FFFFFFF)
        }
        FatType::Fat16 => {
            let base = n as u64 * 2;
            handle.seek(io::SeekFrom::Start(fat_start + base))?;
            let mut buf = [0; 2];
            handle.read_exact(&mut buf)?;
            Ok(u16::from_le_bytes(buf) as u32)
        }
        FatType::Fat12 => {
            let base = n as u64 + (n as u64 / 2);
            handle.seek(io::SeekFrom::Start(fat_start + base))?;
            let mut buf = [0; 2];
            handle.read_exact(&mut buf)?;
            let entry16 = u16::from_le_bytes(buf);
            if n & 1 == 0 {
                Ok((entry16 & 0xFFF) as u32)
            } else {
                Ok((entry16 >> 4) as u32)
            }
        }
    }
}

struct Traverser<'a, H>
where
    H: io::Seek + io::Read,
{
    buf: &'a mut Vec<u8>,
    handle: &'a mut H,
    partition_byte_start: u64,
    bpb: &'a Bpb,
}

impl<'a, H> Traverser<'a, H>
where
    H: io::Seek + io::Read,
{
    fn new(
        handle: &'a mut H,
        buf: &'a mut Vec<u8>,
        bpb: &'a Bpb,
        partition_byte_start: u64,
    ) -> Self {
        Traverser {
            buf,
            handle,
            partition_byte_start,
            bpb,
        }
    }
    fn traverse<'b>(&'b mut self, cluster_index: u32) -> Traverse<'a, 'b, H> {
        Traverse {
            traverser: self,
            current_entry: cluster_index,
        }
    }
}

struct Traverse<'a, 'b, H>
where
    H: io::Seek + io::Read,
{
    traverser: &'b mut Traverser<'a, H>,
    current_entry: u32,
}

impl<'a, 'b, H> Traverse<'a, 'b, H>
where
    H: io::Seek + io::Read,
{
    fn for_each<F, T>(&mut self, mut f: F) -> Result<Option<T>, Error>
    where
        F: FnMut(&[u8]) -> Option<T>,
    {
        loop {
            let entry = match classify_fat_entry(
                self.traverser.bpb.fat_type(),
                self.current_entry,
                self.traverser.bpb.maximum_valid_cluster(),
            )
            .map_err(FatError::FatLookup)?
            {
                FileFatEntry::EndOfFile => break Ok(None),
                FileFatEntry::AllocatedCluster(entry) => entry,
            };
            // Subtract 2 because the first cluster is cluster number 2
            let cluster_start = self.traverser.partition_byte_start
                + self.traverser.bpb.data_offset()
                + (entry as u64 - 2) * self.traverser.bpb.bytes_per_cluster() as u64;
            handle_read(
                self.traverser.handle,
                cluster_start,
                self.traverser.bpb.bytes_per_cluster() as usize,
                &mut self.traverser.buf,
            )?;
            if let Some(t) = f(&self.traverser.buf) {
                break Ok(Some(t));
            }
            let next_entry = fat_entry_of_nth_cluster(
                self.traverser.handle,
                self.traverser.bpb.fat_type(),
                self.traverser.partition_byte_start + self.traverser.bpb.fat_offset(),
                entry,
            )?;
            self.current_entry = next_entry;
        }
    }

    fn write_data<O>(&mut self, size: u32, output: &mut O) -> Result<(), Error>
    where
        O: io::Write,
    {
        let mut remaining = size as usize;
        let maybe_maybe_io_error = self.for_each(|data| {
            if let Some(next_remaining) = remaining.checked_sub(data.len()) {
                remaining = next_remaining;
                output.write_all(data).err().map(Some)
            } else {
                Some(output.write_all(&data[0..remaining]).err())
            }
        })?;
        if let Some(Some(io_error)) = maybe_maybe_io_error {
            Err(io_error.into())
        } else {
            Ok(())
        }
    }
}

fn read_root_directory<H>(traverser: &mut Traverser<H>) -> Result<Directory, Error>
where
    H: io::Seek + io::Read,
{
    match traverser.bpb.fat_type() {
        FatType::Fat32 => Directory::from_traverser(traverser, traverser.bpb.root_cluster),
        FatType::Fat12 | FatType::Fat16 => {
            handle_read(
                traverser.handle,
                traverser.partition_byte_start + traverser.bpb.root_directory_offset(),
                traverser.bpb.root_directory_size(),
                traverser.buf,
            )?;
            let directory = Directory::from_contiguous(traverser.buf);
            Ok(directory)
        }
    }
}

pub enum FatFile {
    Directory(Directory),
    Normal(DirectoryEntry),
}

fn lookup_path<H, P>(traverser: &mut Traverser<H>, path: P) -> Result<FatFile, Error>
where
    H: io::Seek + io::Read,
    P: AsRef<path::Path>,
{
    let mut directory_stack = vec![FatFile::Directory(read_root_directory(traverser)?)];
    for component in path.as_ref().components() {
        use path::Component;
        match component {
            Component::Prefix(_) => return Err(FatError::NoSuchFile.into()),
            Component::CurDir => (),
            Component::ParentDir => {
                directory_stack.pop();
                if directory_stack.is_empty() {
                    return Err(FatError::InvalidPath.into());
                }
            }
            Component::RootDir => {
                if directory_stack.is_empty() {
                    return Err(FatError::InvalidPath.into());
                }
                directory_stack.truncate(1);
            }
            Component::Normal(os_str) => {
                let directory = match directory_stack.last().ok_or(FatError::InvalidPath)? {
                    FatFile::Normal(_) => return Err(FatError::InvalidPath.into()),
                    FatFile::Directory(ref directory) => directory,
                };
                let lookup_path = if let Some(entry) =
                    directory.find_entry(os_str.to_string_lossy().to_string().as_str())
                {
                    if entry.is_directory() {
                        FatFile::Directory(Directory::from_traverser(
                            traverser,
                            entry.first_cluster,
                        )?)
                    } else {
                        FatFile::Normal(entry.clone())
                    }
                } else {
                    return Err(FatError::NoSuchFile.into());
                };
                directory_stack.push(lookup_path);
            }
        }
    }
    Ok(directory_stack.pop().ok_or(FatError::InvalidPath)?)
}

pub fn list_file<H, P>(
    handle: &mut H,
    partition_byte_range: Range<u64>,
    path: P,
) -> Result<FatFile, Error>
where
    H: io::Seek + io::Read,
    P: AsRef<path::Path>,
{
    let mut buf = Vec::new();
    let bpb = Bpb::read(handle, partition_byte_range.start, &mut buf)?;
    let mut traverser = Traverser::new(handle, &mut buf, &bpb, partition_byte_range.start);
    lookup_path(&mut traverser, path)
}

pub fn read_file<H, O, P>(
    handle: &mut H,
    partition_byte_range: Range<u64>,
    path: P,
    output: &mut O,
) -> Result<(), Error>
where
    H: io::Seek + io::Read,
    O: io::Write,
    P: AsRef<path::Path>,
{
    let mut buf = Vec::new();
    let bpb = Bpb::read(handle, partition_byte_range.start, &mut buf)?;
    let mut traverser = Traverser::new(handle, &mut buf, &bpb, partition_byte_range.start);
    match lookup_path(&mut traverser, path)? {
        FatFile::Directory(_) => Err(FatError::ExpectedFileFoundDirectory.into()),
        FatFile::Normal(entry) => traverser
            .traverse(entry.first_cluster)
            .write_data(entry.file_size, output),
    }
}

#[derive(Debug)]
pub struct FatInfo {
    bpb: Bpb,
    fs_info: Option<FsInfo>,
    fs_info_warning: Option<FsInfoWarning>,
}

impl FatInfo {
    pub fn fat_type(&self) -> FatType {
        self.bpb.fat_type()
    }
    pub fn num_clusters(&self) -> u32 {
        self.bpb.count_of_clusters()
    }
}

pub fn fat_info<H>(handle: &mut H, partition_byte_range: Range<u64>) -> Result<FatInfo, Error>
where
    H: io::Seek + io::Read,
{
    let mut buf = Vec::new();
    let bpb = Bpb::read(handle, partition_byte_range.start, &mut buf)?;
    let (fs_info, fs_info_warning) = if let FatType::Fat32 = bpb.fat_type() {
        let (fs_info, fs_info_warning) =
            FsInfo::read(&bpb, handle, partition_byte_range.start, &mut buf)?;
        (Some(fs_info), fs_info_warning)
    } else {
        (None, None)
    };
    Ok(FatInfo {
        bpb,
        fs_info,
        fs_info_warning,
    })
}

#[derive(Debug)]
pub struct PathPair {
    pub in_local_filesystem: File,
    pub in_disk_image: path::PathBuf,
}

fn round_up_to_nearest_cluster_size(size: u64) -> u64 {
    if size % create::BYTES_PER_CLUSTER as u64 == 0 {
        size
    } else {
        size + (create::BYTES_PER_CLUSTER as u64 - (size % create::BYTES_PER_CLUSTER as u64))
    }
}

mod directory_hierarchy {
    use super::{
        create, round_up_to_nearest_cluster_size, Error, FatError, PathPair, DIRECTORY_ENTRY_BYTES,
    };
    use std::collections::BTreeMap;
    use std::fs::File;
    use std::path::{Component, Components};

    pub type Directory<'a, T> = BTreeMap<String, AnnotatedNode<'a, T>>;

    #[derive(Debug)]
    pub enum Node<'a, T> {
        Directory(Directory<'a, T>),
        File(&'a File),
    }

    impl<'a, T> Node<'a, T> {
        fn size_in_clusters(&self) -> Result<u32, Error> {
            let size_in_bytes_rounded_up = match self {
                Node::Directory(directory) => round_up_to_nearest_cluster_size(
                    directory.len() as u64 * DIRECTORY_ENTRY_BYTES as u64,
                ) as u32,
                Node::File(file) => round_up_to_nearest_cluster_size(file.metadata()?.len()) as u32,
            };
            debug_assert!(size_in_bytes_rounded_up % create::BYTES_PER_CLUSTER == 0);
            Ok(size_in_bytes_rounded_up / create::BYTES_PER_CLUSTER)
        }
    }

    #[derive(Debug)]
    pub struct AnnotatedNode<'a, T> {
        pub annotation: T,
        pub node: Node<'a, T>,
    }

    impl<'a> AnnotatedNode<'a, ()> {
        fn empty_directory_unit_annotation() -> Self {
            Self {
                node: Node::Directory(Directory::default()),
                annotation: (),
            }
        }
    }

    fn directory_insert<'a>(
        directory: &mut Directory<'a, ()>,
        current: Component,
        mut rest: Components,
        file: &'a File,
    ) -> Result<(), Error> {
        if let Component::Normal(os_str) = current {
            let name = os_str
                .to_str()
                .ok_or_else(|| {
                    FatError::InvalidDiskPath(
                        "disk image paths must consist of utf-8 characters".to_string(),
                    )
                })?
                .to_string();
            if let Some(next) = rest.next() {
                // current component refers to directory
                match directory
                    .entry(name)
                    .or_insert_with(AnnotatedNode::empty_directory_unit_annotation)
                    .node
                {
                    Node::Directory(ref mut directory) => {
                        directory_insert(directory, next, rest, file)?
                    }
                    Node::File(_) => {
                        return Err(FatError::InvalidDiskPath(
                            "path refers to subdirectory of file".to_string(),
                        )
                        .into())
                    }
                }
            } else {
                // current component refers to file
                if directory.contains_key(&name) {
                    return Err(FatError::InvalidDiskPath(
                        "path refers to existant file or directory".to_string(),
                    )
                    .into());
                }
                directory.insert(
                    name,
                    AnnotatedNode {
                        node: Node::File(file),
                        annotation: (),
                    },
                );
            }
            Ok(())
        } else {
            Err(FatError::InvalidDiskPath(
                "disk image paths must consist of normal components".to_string(),
            )
            .into())
        }
    }

    fn directory_for_each<'a, T, F: FnMut(&AnnotatedNode<'a, T>)>(
        directory: &Directory<'a, T>,
        f: &mut F,
    ) {
        directory.values().for_each(|node| {
            f(node);
            if let Node::Directory(ref subdirectory) = node.node {
                directory_for_each(subdirectory, f);
            }
        })
    }

    #[derive(Debug, Default)]
    pub struct ClusterInfo {
        pub start: u32,
        pub count: u32,
    }

    fn directory_annotate_with_cluster_info<'a>(
        directory: Directory<'a, ()>,
        next_start_cluster: &mut u32,
    ) -> Result<Directory<'a, ClusterInfo>, Error> {
        let mut out: Directory<'a, ClusterInfo> = Default::default();
        for (file_name, annotated_node) in directory {
            let start = *next_start_cluster;
            let count = annotated_node.node.size_in_clusters()?;
            *next_start_cluster += count;
            let annotation = ClusterInfo { start, count };
            let node = match annotated_node.node {
                Node::Directory(directory) => Node::Directory(
                    directory_annotate_with_cluster_info(directory, next_start_cluster)?,
                ),
                Node::File(file) => Node::File(file),
            };
            let annotated_node = AnnotatedNode { node, annotation };
            out.insert(file_name, annotated_node);
        }
        Ok(out)
    }

    #[derive(Debug)]
    pub struct DirectoryHierarchy<'a> {
        root: Directory<'a, ClusterInfo>,
    }

    impl<'a> DirectoryHierarchy<'a> {
        pub fn new<I>(path_pairs: I) -> Result<Self, Error>
        where
            I: IntoIterator<Item = &'a PathPair>,
        {
            let mut root_unsized = Default::default();
            for PathPair {
                in_local_filesystem,
                in_disk_image,
            } in path_pairs
            {
                let mut components = in_disk_image.components();
                let first = components.next().ok_or(FatError::InvalidDiskPath(
                    "path must not be empty".to_string(),
                ))?;
                if first != Component::RootDir {
                    return Err(FatError::InvalidDiskPath(
                        "paths in disk image must start with root".to_string(),
                    )
                    .into());
                }
                let first_non_root = components.next().ok_or(FatError::InvalidDiskPath(
                    "paths must refer to normal file paths - not root directory".to_string(),
                ))?;
                directory_insert(
                    &mut root_unsized,
                    first_non_root,
                    components,
                    &in_local_filesystem,
                )?;
            }
            // The first cluster will be cluster 2. The root directory will always be the first
            // file visited by directory_map.
            let mut next_start_cluster = 2;
            let root = directory_annotate_with_cluster_info(root_unsized, &mut next_start_cluster)?;
            let out = Self { root };
            Ok(out)
        }

        pub fn for_each<F: FnMut(&AnnotatedNode<'a, ClusterInfo>)>(&self, mut f: F) {
            directory_for_each(&self.root, &mut f);
        }

        pub fn implied_num_data_clusters(&self) -> Result<u32, Error> {
            let mut total_file_size = 0;
            let mut error = None;
            self.for_each(|annotated_node| {
                if error.is_none() {
                    match annotated_node.node.size_in_clusters() {
                        Ok(size_in_clusters) => {
                            total_file_size += size_in_clusters * create::BYTES_PER_CLUSTER
                        }
                        Err(e) => error = Some(e),
                    }
                }
            });
            if let Some(e) = error {
                return Err(e);
            }
            debug_assert!(total_file_size % create::BYTES_PER_CLUSTER == 0);
            total_file_size =
                total_file_size.max(create::MIN_NUM_CLUSTERS as u32 * create::BYTES_PER_CLUSTER);
            let num_clusters = total_file_size / create::BYTES_PER_CLUSTER;
            assert!(num_clusters <= u32::MAX);
            Ok(num_clusters as u32)
        }

        pub fn implied_total_num_clusters(&self) -> Result<u64, Error> {
            let num_clusters = self.implied_num_data_clusters()? as u64;
            // Add 2 to the number of clusters to account for the fact that the first 2 FAT entries
            // are unused.
            let fat_size =
                (num_clusters + 2) * create::FAT_ENTRY_SIZE_BYTES as u64 * create::NUM_FATS as u64;
            let reserved_size =
                create::RESERVED_SECTOR_COUNT as u64 * create::BYTES_PER_SECTOR as u64;
            let num_bytes = num_clusters * create::BYTES_PER_CLUSTER as u64
                + round_up_to_nearest_cluster_size(fat_size)
                + reserved_size;
            let num_clusters = num_bytes / (create::BYTES_PER_CLUSTER as u64);
            Ok(num_clusters)
        }
    }
}

pub fn partition_size<'a, I>(path_pairs: I) -> Result<u64, Error>
where
    I: IntoIterator<Item = &'a PathPair>,
{
    use directory_hierarchy::DirectoryHierarchy;
    let num_clusters = DirectoryHierarchy::new(path_pairs)?.implied_total_num_clusters()?;
    Ok(num_clusters * create::BYTES_PER_CLUSTER as u64)
}

fn write_zero_sector<H>(handle: &mut H) -> Result<(), Error>
where
    H: io::Write,
{
    handle.write_all(&[0; create::BYTES_PER_SECTOR as usize])?;
    Ok(())
}

fn write_fat32_fat<H>(
    handle: &mut H,
    directory_hierarchy: &directory_hierarchy::DirectoryHierarchy,
) -> Result<(), Error>
where
    H: io::Write,
{
    const FAT32_ENTRY_END_OF_FILE: u32 = 0xFFFFFFFF;
    let mut error = None;
    // Skip the first 2 entries
    handle.write_all(&0u32.to_le_bytes())?;
    handle.write_all(&0u32.to_le_bytes())?;
    let mut entry_count = 2;
    directory_hierarchy.for_each(|annotated_node| {
        if error.is_some() {
            return;
        }
        for cluster_index in (annotated_node.annotation.start + 1)
            ..(annotated_node.annotation.start + annotated_node.annotation.count)
        {
            if let Err(e) = handle.write_all(&cluster_index.to_le_bytes()) {
                error = Some(e);
                return;
            }
        }
        if let Err(e) = handle.write_all(&FAT32_ENTRY_END_OF_FILE.to_le_bytes()) {
            error = Some(e);
        }
        entry_count += annotated_node.annotation.count;
    });
    if let Some(e) = error {
        return Err(e.into());
    }
    let remaining_fat_entry_count = directory_hierarchy.implied_num_data_clusters()? - entry_count;
    for _ in 0..remaining_fat_entry_count {
        handle.write_all(&0u32.to_le_bytes())?;
    }
    Ok(())
}

pub fn write_partition<'a, H, I>(handle: &mut H, path_pairs: I) -> Result<(), Error>
where
    H: io::Write,
    I: IntoIterator<Item = &'a PathPair>,
{
    use directory_hierarchy::DirectoryHierarchy;
    let hierarchy = DirectoryHierarchy::new(path_pairs)?;
    let bpb_raw = Bpb::new_fat32_raw(&hierarchy)?;
    let bpb = match Bpb::parse(&bpb_raw) {
        Ok(bpb) => bpb,
        Err(ref e) => {
            eprintln!("Failed to parse generated BPB");
            die(e);
        }
    };
    let _fat_type = bpb.fat_type(); // sanity check
    let fs_info_raw = FsInfo::new_raw(0, hierarchy.implied_num_data_clusters()?);
    if let Err(ref e) = FsInfo::parse(&fs_info_raw) {
        eprintln!("Failed to parse generated FsInfo");
        die(e);
    }
    handle.write_all(&bpb_raw)?; // sector 0
    handle.write_all(&fs_info_raw)?; // sector 1
    for _ in 2..create::BK_BOOT_SECTOR {
        write_zero_sector(handle)?;
    }
    handle.write_all(&bpb_raw)?; // sector 6
    handle.write_all(&fs_info_raw)?; // sector 7
    for _ in 0..create::NUM_FATS {
        write_fat32_fat(handle, &hierarchy)?;
    }
    Ok(())
}

fn die(error: &Error) -> ! {
    eprintln!("{}", error);
    #[cfg(feature = "backtrace")]
    eprintln!("{}", error.backtrace());
    std::process::exit(1);
}
