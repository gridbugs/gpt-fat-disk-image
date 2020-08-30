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
    Fat12OrFat16(BpbFat12OrFat16),
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
    InvalidSignature(u16),
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

impl Bpb {
    pub(crate) fn new(raw: &[u8]) -> Result<Self, BpbError> {
        let general = BpbGeneral::new(raw)?;
        if general.total_sectors_16 != 0 && general.total_sectors_32 == 0 {
            let specific = BpbFat12OrFat16Specific::new(raw)?;
            Ok(Bpb::Fat12OrFat16(BpbFat12OrFat16 { general, specific }))
        } else if general.total_sectors_16 == 0 && general.total_sectors_32 != 0 {
            let specific = BpbFat32Specific::new(raw)?;
            Ok(Bpb::Fat32(BpbFat32 { general, specific }))
        } else {
            Err(BpbError::ExactlyOneTotalSectorsFieldMustBeZero {
                total_sectors_16: general.total_sectors_16,
                total_sectors_32: general.total_sectors_32,
            })
        }
    }
}
