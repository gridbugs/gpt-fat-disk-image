use std::fs::File;
use std::io;
use std::ops::Range;
use std::path;

mod create {
    pub const BYTES_PER_SECTOR: u16 = 512;
    pub const SECTORS_PER_CLUSTER: u8 = 1;
    pub const BYTES_PER_CLUSTER: u32 = BYTES_PER_SECTOR as u32 * SECTORS_PER_CLUSTER as u32;
    pub const FAT_ENTRY_SIZE: u8 = 32; // all created images will be formatted as FAT32
    pub const MIN_NUM_CLUSTERS: u64 = 65526; // 1 larger than the technical minimum for FAT32 to mitigate off-by-1 errors in firmware
    pub const NUM_FATS: u8 = 2;
    pub const RESERVED_SECTOR_COUNT: u16 = 32;
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
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
                return Err(Error::BpbDoesNotMatchBackupBpb);
            }
        }
        Ok(bpb)
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
                    return Err(Error::UnexpectedNonZero { byte_index: i });
                }
            }
            drive_number = raw[64];
            if raw[65] != 0 {
                return Err(Error::UnexpectedNonZero { byte_index: 65 });
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
                return Err(Error::UnexpectedNonZero { byte_index: 37 });
            }
            boot_signature = raw[38];
            volume_id = u32::from_le_bytes(raw[39..43].try_into().unwrap());
            volume_label = String::from_utf8_lossy(&raw[43..54]).to_string();
            file_system_type = String::from_utf8_lossy(&raw[54..62]).to_string();
        } else {
            return Err(Error::ExactlyOneTotalSectorsFieldMustBeZero {
                total_sectors_16,
                total_sectors_32,
            });
        }
        if (fat_size_16 == 0) == (fat_size_32 == 0) {
            return Err(Error::ExactlyOneFatSizeMustBeZero {
                fat_size_16,
                fat_size_32,
            });
        }
        let signature = u16::from_le_bytes(raw[510..512].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(Error::InvalidSignature(signature));
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
            Err(Error::InvalidFsInfoLeadSignature(_)) => Some(FsInfoWarning::NoBackup),
            Err(other) => return Err(other),
        };
        Ok((fs_info, warning))
    }

    fn parse(raw: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let lead_signature = u32::from_le_bytes(raw[0..4].try_into().unwrap());
        if lead_signature != FS_INFO_REQUIRED_LEAD_SIGNATURE {
            return Err(Error::InvalidFsInfoLeadSignature(lead_signature));
        }
        let struc_signature = u32::from_le_bytes(raw[484..488].try_into().unwrap());
        if struc_signature != FS_INFO_REQUIRED_STRUC_SIGNATURE {
            return Err(Error::InvalidFsInfoStrucSignature(struc_signature));
        }
        let free_count = u32::from_le_bytes(raw[488..492].try_into().unwrap());
        let next_free = u32::from_le_bytes(raw[492..496].try_into().unwrap());
        let trail_signature = u32::from_le_bytes(raw[508..512].try_into().unwrap());
        if trail_signature != FS_INFO_REQUIRED_TRAIL_SIGNATURE {
            return Err(Error::InvalidFsInfoTrailSignature(trail_signature));
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

#[derive(Debug)]
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
    handle
        .seek(io::SeekFrom::Start(offset))
        .map_err(Error::Io)?;
    handle.read_exact(buf).map_err(Error::Io)?;
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
    debug_assert!(n >= 2);
    match fat_type {
        FatType::Fat32 => {
            let base = n as u64 * 4;
            handle
                .seek(io::SeekFrom::Start(fat_start + base))
                .map_err(Error::Io)?;
            let mut buf = [0; 4];
            handle.read_exact(&mut buf).map_err(Error::Io)?;
            Ok(u32::from_le_bytes(buf) & 0x0FFFFFFF)
        }
        FatType::Fat16 => {
            let base = n as u64 * 2;
            handle
                .seek(io::SeekFrom::Start(fat_start + base))
                .map_err(Error::Io)?;
            let mut buf = [0; 2];
            handle.read_exact(&mut buf).map_err(Error::Io)?;
            Ok(u16::from_le_bytes(buf) as u32)
        }
        FatType::Fat12 => {
            let base = n as u64 + (n as u64 / 2);
            handle
                .seek(io::SeekFrom::Start(fat_start + base))
                .map_err(Error::Io)?;
            let mut buf = [0; 2];
            handle.read_exact(&mut buf).map_err(Error::Io)?;
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
            .map_err(Error::FatLookup)?
            {
                FileFatEntry::EndOfFile => break Ok(None),
                FileFatEntry::AllocatedCluster(entry) => entry,
            };
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
            Err(Error::Io(io_error))
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
            Component::Prefix(_) => return Err(Error::NoSuchFile),
            Component::CurDir => (),
            Component::ParentDir => {
                directory_stack.pop();
                if directory_stack.is_empty() {
                    return Err(Error::InvalidPath);
                }
            }
            Component::RootDir => {
                if directory_stack.is_empty() {
                    return Err(Error::InvalidPath);
                }
                directory_stack.truncate(1);
            }
            Component::Normal(os_str) => {
                let directory = match directory_stack.last().ok_or(Error::InvalidPath)? {
                    FatFile::Normal(_) => return Err(Error::InvalidPath),
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
                    return Err(Error::NoSuchFile);
                };
                directory_stack.push(lookup_path);
            }
        }
    }
    Ok(directory_stack.pop().ok_or(Error::InvalidPath)?)
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
        FatFile::Directory(_) => Err(Error::ExpectedFileFoundDirectory),
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

mod directory_hierarchy {
    use super::{Error, PathPair};
    use std::collections::HashMap;
    use std::fs::File;
    use std::path::{Component, Components};

    pub type Directory<'a> = HashMap<String, Node<'a>>;

    pub enum Node<'a> {
        Directory(Directory<'a>),
        File(&'a File),
    }

    fn directory_insert<'a>(
        directory: &mut Directory<'a>,
        current: Component,
        mut rest: Components,
        file: &'a File,
    ) -> Result<(), Error> {
        if let Component::Normal(os_str) = current {
            let name = os_str
                .to_str()
                .ok_or_else(|| {
                    Error::InvalidDiskPath(
                        "disk image paths must consist of utf-8 characters".to_string(),
                    )
                })?
                .to_string();
            if let Some(next) = rest.next() {
                // current component refers to directory
                match directory
                    .entry(name)
                    .or_insert_with(|| Node::Directory(Directory::default()))
                {
                    Node::Directory(directory) => directory_insert(directory, next, rest, file)?,
                    Node::File(_) => {
                        return Err(Error::InvalidDiskPath(
                            "path refers to subdirectory of file".to_string(),
                        ))
                    }
                }
            } else {
                // current component refers to file
                if directory.contains_key(&name) {
                    return Err(Error::InvalidDiskPath(
                        "path refers to existant file or directory".to_string(),
                    ));
                }
                directory.insert(name, Node::File(file));
            }
            Ok(())
        } else {
            Err(Error::InvalidDiskPath(
                "disk image paths must consist of normal components".to_string(),
            ))
        }
    }

    fn directory_for_each<'a, F: FnMut(&Node<'a>)>(directory: &Directory<'a>, f: &mut F) {
        directory.values().for_each(|node| {
            f(node);
            if let Node::Directory(subdirectory) = node {
                directory_for_each(subdirectory, f);
            }
        })
    }

    pub struct DirectoryHierarchy<'a> {
        root: Directory<'a>,
    }

    impl<'a> DirectoryHierarchy<'a> {
        pub fn new<I>(path_pairs: I) -> Result<Self, Error>
        where
            I: IntoIterator<Item = &'a PathPair>,
        {
            let mut root = HashMap::new();
            for PathPair {
                in_local_filesystem,
                in_disk_image,
            } in path_pairs
            {
                let mut components = in_disk_image.components();
                let first = components
                    .next()
                    .ok_or(Error::InvalidDiskPath("path must not be empty".to_string()))?;
                if first != Component::RootDir {
                    return Err(Error::InvalidDiskPath(
                        "paths in disk image must start with root".to_string(),
                    ));
                }
                let first_non_root = components.next().ok_or(Error::InvalidDiskPath(
                    "paths must refer to normal file paths - not root directory".to_string(),
                ))?;
                directory_insert(&mut root, first_non_root, components, &in_local_filesystem)?;
            }
            Ok(Self { root })
        }

        pub fn for_each<F: FnMut(&Node<'a>)>(&self, mut f: F) {
            directory_for_each(&self.root, &mut f);
        }
    }
}

pub fn partition_size<'a, I>(path_pairs: I) -> Result<u64, Error>
where
    I: IntoIterator<Item = &'a PathPair>,
{
    use directory_hierarchy::{DirectoryHierarchy, Node};
    fn round_up_to_nearest_cluster_size(size: u64) -> u64 {
        if size % create::BYTES_PER_CLUSTER as u64 == 0 {
            size
        } else {
            size + (create::BYTES_PER_CLUSTER as u64 - (size % create::BYTES_PER_CLUSTER as u64))
        }
    }
    let path_pairs = path_pairs.into_iter().collect::<Vec<_>>();
    let total_file_size = {
        let hierarchy = DirectoryHierarchy::new(path_pairs.iter().cloned())?;
        let mut total_file_size = 0;
        let mut error = None;
        hierarchy.for_each(|node| {
            if error.is_none() {
                match node {
                    Node::File(file) => match file.metadata() {
                        Ok(metadata) => {
                            total_file_size += round_up_to_nearest_cluster_size(metadata.len())
                        }
                        Err(e) => error = Some(Error::Io(e)),
                    },
                    Node::Directory(directory) => {
                        total_file_size += round_up_to_nearest_cluster_size(
                            directory.len() as u64 * DIRECTORY_ENTRY_BYTES as u64,
                        )
                    }
                }
            }
        });
        if let Some(e) = error {
            return Err(e);
        }
        total_file_size.max(create::MIN_NUM_CLUSTERS * create::BYTES_PER_CLUSTER as u64)
    };
    debug_assert!(total_file_size % create::BYTES_PER_CLUSTER as u64 == 0);
    let num_clusters = total_file_size / create::BYTES_PER_CLUSTER as u64;
    let fat_size = num_clusters * create::FAT_ENTRY_SIZE as u64 * create::NUM_FATS as u64;
    let reserved_size = create::RESERVED_SECTOR_COUNT as u64 * create::BYTES_PER_SECTOR as u64;
    Ok(total_file_size + round_up_to_nearest_cluster_size(fat_size) + reserved_size)
}
