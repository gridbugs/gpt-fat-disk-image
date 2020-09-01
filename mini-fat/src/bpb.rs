use crate::directory::{Directory, DIRECTORY_ENTRY_BYTES};

pub struct Bpb2 {
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

#[derive(Debug)]
pub struct BpbGeneral {
    pub jmp_boot: [u8; 3],
    pub oem_name: String,
    pub bytes_per_sector: u16,
    pub sectors_per_cluster: u8,
    pub reserved_sector_count: u16,
    pub num_fats: u8,
    pub root_entry_count: u16,
    pub total_sectors_16: u16,
    pub media: u8,
    pub fat_size_16: u16,
    pub sectors_per_track: u16,
    pub num_heads: u16,
    pub hidden_sectors: u32,
    pub total_sectors_32: u32,
}

#[derive(Debug)]
pub struct BpbFat12OrFat16Specific {
    pub drive_number: u8,
    pub boot_signature: u8,
    pub volume_id: u32,
    pub volume_label: String,
    pub file_system_type: String,
    pub signature: u16,
}

#[derive(Debug)]
pub struct BpbFat32Specific {
    pub fat_size_32: u32,
    pub ext_flags: u16,
    pub fs_version: u16,
    pub root_cluster: u32,
    pub fs_info: u16,
    pub bk_boot_sector: u16,
    pub drive_number: u8,
    pub boot_signature: u8,
    pub volume_id: u32,
    pub volume_label: String,
    pub file_system_type: String,
    pub signature: u16,
}

#[derive(Debug)]
pub struct BpbFat12OrFat16 {
    pub general: BpbGeneral,
    pub specific: BpbFat12OrFat16Specific,
}

#[derive(Debug)]
pub struct BpbFat32 {
    pub general: BpbGeneral,
    pub specific: BpbFat32Specific,
}

#[derive(Debug)]
pub enum Bpb {
    Fat12(BpbFat12OrFat16),
    Fat16(BpbFat12OrFat16),
    Fat32(BpbFat32),
}

const REQUIRED_SIGNATURE: u16 = 0xAA55;

#[derive(Debug, Clone, Copy)]
pub enum BpbError {
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
    TooManyClustersForNonFat32Header,
}

impl Bpb2 {
    pub fn parse(raw: &[u8]) -> Result<Self, BpbError> {
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
        if (total_sectors_16 != 0) && (total_sectors_32 == 0) {
            // FAT32
            fat_size_32 = u32::from_le_bytes(raw[36..40].try_into().unwrap());
            ext_flags = u16::from_le_bytes(raw[40..42].try_into().unwrap());
            fs_version = u16::from_le_bytes(raw[42..44].try_into().unwrap());
            root_cluster = u32::from_le_bytes(raw[44..48].try_into().unwrap());
            fs_info = u16::from_le_bytes(raw[48..50].try_into().unwrap());
            bk_boot_sector = u16::from_le_bytes(raw[50..52].try_into().unwrap());
            for i in 52..64 {
                if raw[i] != 0 {
                    return Err(BpbError::UnexpectedNonZero { byte_index: i });
                }
            }
            drive_number = raw[64];
            if raw[65] != 0 {
                return Err(BpbError::UnexpectedNonZero { byte_index: 65 });
            }
            boot_signature = raw[66];
            volume_id = u32::from_le_bytes(raw[67..71].try_into().unwrap());
            volume_label = String::from_utf8_lossy(&raw[71..82]).to_string();
            file_system_type = String::from_utf8_lossy(&raw[82..90]).to_string();
        } else if (total_sectors_16 == 0) && (total_sectors_32 != 0) {
            // FAT12 or FAT16
            fat_size_32 = 0;
            ext_flags = 0;
            fs_version = 0;
            root_cluster = 0;
            fs_info = 0;
            bk_boot_sector = 0;
            drive_number = raw[36];
            if raw[37] != 0 {
                return Err(BpbError::UnexpectedNonZero { byte_index: 37 });
            }
            boot_signature = raw[38];
            volume_id = u32::from_le_bytes(raw[39..43].try_into().unwrap());
            volume_label = String::from_utf8_lossy(&raw[43..54]).to_string();
            file_system_type = String::from_utf8_lossy(&raw[54..62]).to_string();
        } else {
            return Err(BpbError::ExactlyOneTotalSectorsFieldMustBeZero {
                total_sectors_16,
                total_sectors_32,
            });
        }
        if (fat_size_16 == 0) == (fat_size_32 == 0) {
            return Err(BpbError::ExactlyOneFatSizeMustBeZero {
                fat_size_16,
                fat_size_32,
            });
        }
        let signature = u16::from_le_bytes(raw[510..512].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(BpbError::InvalidSignature(signature));
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

    fn total_sectors(&self) -> u32 {
        if self.total_sectors_16 != 0 && self.total_sectors_32 == 0 {
            self.total_sectors_16 as u32
        } else {
            debug_assert!(self.total_sectors_16 == 0 && self.total_sectors_32 != 0);
            self.total_sectors_32
        }
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

    fn root_directory_num_bytes(&self) -> usize {
        debug_assert!((self.fat_type() == FatType::Fat32) == (self.root_entry_count == 0));
        self.root_entry_count as usize * DIRECTORY_ENTRY_BYTES
    }

    fn fat_byte_range(&self) -> std::ops::Range<usize> {
        let start = self.reserved_sector_count as usize * self.bytes_per_sector as usize;
        let end = start + (self.fat_size_in_sectors() as usize * self.bytes_per_sector as usize);
        start..end
    }

    fn cluster_byte_offset(&self) -> usize {
        self.root_directory_num_bytes()
            + ((self.reserved_sector_count as usize
                + self.fat_size_in_sectors() as usize * self.num_fats as usize)
                * self.bytes_per_sector as usize)
    }
}

impl BpbGeneral {
    fn new(raw: &[u8]) -> Result<Self, BpbError> {
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
        if (total_sectors_16 == 0) == (total_sectors_32 == 0) {
            return Err(BpbError::ExactlyOneTotalSectorsFieldMustBeZero {
                total_sectors_16,
                total_sectors_32,
            });
        };
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
        })
    }

    pub fn total_sectors(&self) -> u32 {
        if self.total_sectors_16 != 0 && self.total_sectors_32 == 0 {
            self.total_sectors_16 as u32
        } else {
            assert!(self.total_sectors_16 == 0 && self.total_sectors_32 != 0);
            self.total_sectors_32
        }
    }

    fn count_of_clusters(&self, fat_size: u32) -> u32 {
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
                + (self.num_fats as u32 * fat_size)
                + root_dir_sectors);
        data_sectors / self.sectors_per_cluster as u32
    }
}

impl BpbFat12OrFat16Specific {
    fn new(raw: &[u8]) -> Result<Self, BpbError> {
        use std::convert::TryInto;
        let drive_number = raw[36];
        if raw[37] != 0 {
            return Err(BpbError::UnexpectedNonZero { byte_index: 37 });
        }
        let boot_signature = raw[38];
        let volume_id = u32::from_le_bytes(raw[39..43].try_into().unwrap());
        let volume_label = String::from_utf8_lossy(&raw[43..54]).to_string();
        let file_system_type = String::from_utf8_lossy(&raw[54..62]).to_string();
        let signature = u16::from_le_bytes(raw[510..512].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(BpbError::InvalidSignature(signature));
        }
        Ok(Self {
            drive_number,
            boot_signature,
            volume_id,
            volume_label,
            file_system_type,
            signature,
        })
    }
}

impl BpbFat32Specific {
    fn new(raw: &[u8]) -> Result<Self, BpbError> {
        use std::convert::TryInto;
        let fat_size_32 = u32::from_le_bytes(raw[36..40].try_into().unwrap());
        let ext_flags = u16::from_le_bytes(raw[40..42].try_into().unwrap());
        let fs_version = u16::from_le_bytes(raw[42..44].try_into().unwrap());
        let root_cluster = u32::from_le_bytes(raw[44..48].try_into().unwrap());
        let fs_info = u16::from_le_bytes(raw[48..50].try_into().unwrap());
        let bk_boot_sector = u16::from_le_bytes(raw[50..52].try_into().unwrap());
        for i in 52..64 {
            if raw[i] != 0 {
                return Err(BpbError::UnexpectedNonZero { byte_index: i });
            }
        }
        let drive_number = raw[64];
        if raw[65] != 0 {
            return Err(BpbError::UnexpectedNonZero { byte_index: 65 });
        }
        let boot_signature = raw[66];
        let volume_id = u32::from_le_bytes(raw[67..71].try_into().unwrap());
        let volume_label = String::from_utf8_lossy(&raw[71..82]).to_string();
        let file_system_type = String::from_utf8_lossy(&raw[82..90]).to_string();
        let signature = u16::from_le_bytes(raw[510..512].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(BpbError::InvalidSignature(signature));
        }
        Ok(Self {
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
}

impl BpbFat12OrFat16 {
    fn new(general: BpbGeneral, specific: BpbFat12OrFat16Specific) -> Result<Self, BpbError> {
        if general.fat_size_16 == 0 {
            return Err(BpbError::ExactlyOneFatSizeMustBeZero {
                fat_size_16: 0,
                fat_size_32: 0,
            });
        }
        Ok(Self { general, specific })
    }
    fn into_bpb(self) -> Result<Bpb, BpbError> {
        let fat_size = self.general.fat_size_16 as u32;
        let count_of_clusters = self.general.count_of_clusters(fat_size);
        if count_of_clusters < 4085 {
            Ok(Bpb::Fat12(self))
        } else if count_of_clusters < 65525 {
            Ok(Bpb::Fat16(self))
        } else {
            Err(BpbError::TooManyClustersForNonFat32Header)
        }
    }
}

impl Bpb {
    pub(crate) fn new(raw: &[u8]) -> Result<Self, BpbError> {
        let general = BpbGeneral::new(raw)?;
        if general.total_sectors_16 != 0 {
            let specific = BpbFat12OrFat16Specific::new(raw)?;
            BpbFat12OrFat16::new(general, specific)?.into_bpb()
        } else {
            let specific = BpbFat32Specific::new(raw)?;
            if general.fat_size_16 != 0 {
                return Err(BpbError::ExactlyOneFatSizeMustBeZero {
                    fat_size_16: general.fat_size_16,
                    fat_size_32: specific.fat_size_32,
                });
            }
            Ok(Bpb::Fat32(BpbFat32 { general, specific }))
        }
    }

    fn general(&self) -> &BpbGeneral {
        match self {
            Self::Fat32(f) => &f.general,
            Self::Fat16(f) | Self::Fat12(f) => &f.general,
        }
    }

    fn fat_size_in_sectors(&self) -> u32 {
        match self {
            Self::Fat32(f) => f.specific.fat_size_32,
            Self::Fat16(f) | Self::Fat12(f) => f.general.fat_size_16 as u32,
        }
    }

    fn count_of_clusters(&self) -> u32 {
        self.general().count_of_clusters(self.fat_size_in_sectors())
    }

    pub fn maximum_valid_cluster(&self) -> u32 {
        self.count_of_clusters() + 1
    }

    fn fat_start_byte_offset_range(&self) -> std::ops::Range<usize> {
        let start = self.general().reserved_sector_count as usize
            * self.general().bytes_per_sector as usize;
        let end = start
            + (self.fat_size_in_sectors() as usize * self.general().bytes_per_sector as usize);
        start..end
    }

    pub fn fat_raw<'a>(&self, raw: &'a [u8]) -> &'a [u8] {
        &raw[self.fat_start_byte_offset_range()]
    }

    pub fn fat_entry_of_nth_cluster(&self, fat_raw: &[u8], n: u32) -> u32 {
        fat_entry_of_nth_cluster(self.fat_type(), fat_raw, n)
    }

    fn root_directory_num_bytes(&self) -> usize {
        match self {
            Self::Fat32(_) => 0,
            Self::Fat16(f) | Self::Fat12(f) => {
                f.general.root_entry_count as usize * DIRECTORY_ENTRY_BYTES
            }
        }
    }

    pub fn clusters_raw<'a>(&self, raw: &'a [u8]) -> &'a [u8] {
        let start = ((self.general().reserved_sector_count as usize
            + self.fat_size_in_sectors() as usize * self.general().num_fats as usize)
            * self.general().bytes_per_sector as usize)
            + self.root_directory_num_bytes();
        &raw[start..]
    }

    pub fn bytes_per_cluster(&self) -> usize {
        self.general().bytes_per_sector as usize * self.general().sectors_per_cluster as usize
    }

    pub fn data_of_nth_cluster<'a>(&self, clusters_raw: &'a [u8], n: u32) -> &'a [u8] {
        assert!(n >= 2);
        let start = (n as usize - 2) * self.bytes_per_cluster();
        let end = start + self.bytes_per_cluster();
        &clusters_raw[start..end]
    }

    pub fn data_starting_at_nth_cluster_cluster_iter<'a>(
        &'a self,
        fat_raw: &'a [u8],
        clusters_raw: &'a [u8],
        n: u32,
    ) -> impl 'a + Iterator<Item = &'a [u8]> {
        FatClusterIndexIter {
            fat_type: self.fat_type(),
            fat_raw,
            maximum_valid_cluster: self.maximum_valid_cluster(),
            current_entry: n,
        }
        .map(move |cluster_index_result| {
            self.data_of_nth_cluster(clusters_raw, cluster_index_result.unwrap())
        })
    }

    pub fn directory_in_nth_cluster_tmp(&self, clusters_raw: &[u8], n: u32) -> Directory {
        Directory::from_contiguous(self.data_of_nth_cluster(clusters_raw, n))
    }

    pub fn root_directory(&self, raw: &[u8]) -> Directory {
        match self {
            Self::Fat32(f) => {
                let fat_raw = self.fat_raw(raw);
                let clusters_raw = self.clusters_raw(raw);
                let iter = self.data_starting_at_nth_cluster_cluster_iter(
                    fat_raw,
                    clusters_raw,
                    f.specific.root_cluster,
                );
                Directory::from_cluster_into_iter(iter)
            }
            Self::Fat16(f) | Self::Fat12(f) => {
                let root_directory_sector = f.general.reserved_sector_count as usize
                    + (f.general.num_fats as usize * f.general.fat_size_16 as usize);
                let root_directory_start_index =
                    root_directory_sector * f.general.bytes_per_sector as usize;
                let root_directory_end_index = root_directory_start_index
                    + (f.general.root_entry_count as usize * DIRECTORY_ENTRY_BYTES);
                Directory::from_contiguous(
                    &raw[root_directory_start_index..root_directory_end_index],
                )
            }
        }
    }

    fn fat_type(&self) -> FatType {
        match self {
            Self::Fat12(_) => FatType::Fat12,
            Self::Fat16(_) => FatType::Fat16,
            Self::Fat32(_) => FatType::Fat32,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FatType {
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

    fn fat_entry_end_of_file(self) -> u32 {
        match self {
            Self::Fat12 => 0xFFF,
            Self::Fat16 => 0xFFFF,
            Self::Fat32 => 0xFFFFFFFF,
        }
    }
}

fn fat_entry_of_nth_cluster(fat_type: FatType, fat_raw: &[u8], n: u32) -> u32 {
    assert!(n >= 2);
    use std::convert::TryInto;
    match fat_type {
        FatType::Fat32 => {
            let base = n as usize * 4;
            u32::from_le_bytes(fat_raw[base..(base + 4)].try_into().unwrap()) & 0x0FFFFFFF
        }
        FatType::Fat16 => {
            let base = n as usize * 2;
            u16::from_le_bytes(fat_raw[base..(base + 2)].try_into().unwrap()) as u32
        }
        FatType::Fat12 => {
            let base = n as usize + (n as usize / 2);
            let entry16 = u16::from_le_bytes(fat_raw[base..(base + 2)].try_into().unwrap());
            if n & 1 == 0 {
                (entry16 & 0xFFF) as u32
            } else {
                (entry16 >> 4) as u32
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

struct FatClusterIndexIter<'a> {
    fat_type: FatType,
    fat_raw: &'a [u8],
    maximum_valid_cluster: u32,
    current_entry: u32,
}

impl<'a> Iterator for FatClusterIndexIter<'a> {
    type Item = Result<u32, FatLookupError>;
    fn next(&mut self) -> Option<Self::Item> {
        let index = match classify_fat_entry(
            self.fat_type,
            self.current_entry,
            self.maximum_valid_cluster,
        ) {
            Err(e) => {
                // set end of file so the iterator stops next time it's polled
                self.current_entry = self.fat_type.fat_entry_end_of_file();
                return Some(Err(e));
            }
            Ok(FileFatEntry::EndOfFile) => return None,
            Ok(FileFatEntry::AllocatedCluster(index)) => index,
        };
        self.current_entry = fat_entry_of_nth_cluster(self.fat_type, self.fat_raw, index);
        Some(Ok(index))
    }
}
