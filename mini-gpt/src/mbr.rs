use std::fmt;

pub const PARTITION_RECORD_COUNT: usize = 4;

#[derive(Debug, Default, Clone, Copy)]
pub struct Mbr<'a> {
    pub boot_code: &'a [u8],
    pub unique_mbr_disk_signature: u32,
    pub partition_record: [MbrPartitionRecord; PARTITION_RECORD_COUNT],
    pub signature: u16,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct MbrPartitionRecord {
    pub boot_indicator: u8,
    pub starting_chs: u32,
    pub os_type: u8,
    pub ending_chs: u32,
    pub starting_lba: u32,
    pub size_in_lba: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct InvalidSignature(pub u16);
pub const REQUIRED_SIGNATURE: u16 = 0xAA55;

impl<'a> Mbr<'a> {
    pub fn from_logical_block(logical_block: &'a [u8]) -> Result<Self, InvalidSignature> {
        use std::convert::TryInto;
        pub const BOOT_CODE_SIZE: usize = 440;
        const UNIQUE_MBR_SIGNATURE_OFFSET: usize = BOOT_CODE_SIZE;
        const PARTITION_RECORD_OFFSET: usize = 446;
        const PARTITION_RECORD_SIZE: usize = 16;
        const SIGNATURE_OFFSET: usize = 510;
        let boot_code = &logical_block[0..BOOT_CODE_SIZE];
        let unique_mbr_disk_signature = u32::from_le_bytes(
            logical_block[UNIQUE_MBR_SIGNATURE_OFFSET..(UNIQUE_MBR_SIGNATURE_OFFSET + 4)]
                .try_into()
                .unwrap(),
        );
        let partition_record = {
            let mut partition_record = [MbrPartitionRecord::default(); PARTITION_RECORD_COUNT];
            for i in 0..PARTITION_RECORD_COUNT {
                let base = PARTITION_RECORD_OFFSET + i * PARTITION_RECORD_SIZE;
                let partition_record_bytes = &logical_block[base..(base + PARTITION_RECORD_SIZE)];
                partition_record[i] = MbrPartitionRecord {
                    boot_indicator: partition_record_bytes[0],
                    starting_chs: partition_record_bytes[1] as u32
                        | ((partition_record_bytes[2] as u32) << 8)
                        | ((partition_record_bytes[3] as u32) << 16),
                    os_type: partition_record_bytes[4],
                    ending_chs: partition_record_bytes[5] as u32
                        | ((partition_record_bytes[6] as u32) << 8)
                        | ((partition_record_bytes[7] as u32) << 16),
                    starting_lba: partition_record_bytes[8] as u32
                        | ((partition_record_bytes[9] as u32) << 8)
                        | ((partition_record_bytes[10] as u32) << 16)
                        | ((partition_record_bytes[11] as u32) << 24),
                    size_in_lba: partition_record_bytes[12] as u32
                        | ((partition_record_bytes[13] as u32) << 8)
                        | ((partition_record_bytes[14] as u32) << 16)
                        | ((partition_record_bytes[15] as u32) << 24),
                };
            }
            partition_record
        };
        let signature = logical_block[SIGNATURE_OFFSET] as u16
            | ((logical_block[SIGNATURE_OFFSET + 1] as u16) << 8);
        if signature == REQUIRED_SIGNATURE {
            Ok(Self {
                boot_code,
                unique_mbr_disk_signature,
                partition_record,
                signature,
            })
        } else {
            Err(InvalidSignature(signature))
        }
    }
}

impl<'a> fmt::Display for Mbr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "Boot Code:")?;
        mini_hex_dump::display_bytes(self.boot_code, 32, f)?;
        writeln!(f, "Disk Signature: {:X}", self.unique_mbr_disk_signature)?;
        writeln!(f, "Partition Record:\n{:#X?}", self.partition_record)?;
        writeln!(f, "Signature: {:X}", self.signature)?;
        Ok(())
    }
}
