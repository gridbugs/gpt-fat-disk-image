pub use anyhow::Error;
use std::fmt;
use std::io;
use std::ops::Range;
use uuid::Uuid;

mod crc32;

mod guid {
    use uuid::Uuid;

    pub fn guid_to_uuid(guid: u128) -> Uuid {
        let d1 = guid as u32;
        let d2 = (guid >> 32) as u16;
        let d3 = (guid >> 48) as u16;
        let d4 = ((guid >> 64) as u64).to_le_bytes();
        Uuid::from_fields(d1, d2, d3, &d4).unwrap()
    }

    pub fn uuid_to_guid(uuid: Uuid) -> u128 {
        let (d1, d2, d3, d4) = uuid.as_fields();
        d1 as u128
            | ((d2 as u128) << 32)
            | ((d3 as u128) << 48)
            | ((u64::from_le_bytes(*d4) as u128) << 64)
    }

    #[cfg(test)]
    mod test {
        #[test]
        fn round_trip() {
            let uuid = super::Uuid::parse_str("C12A7328-F81F-11D2-BA4B-00A0C93EC93B").unwrap();
            let guid = super::uuid_to_guid(uuid);
            let uuid_round_tripped = super::guid_to_uuid(guid);
            assert_eq!(uuid, uuid_round_tripped);
        }
    }
}

#[derive(Debug)]
pub enum GptError {
    InvalidSignature(u64),
    IncorrectRevision(u32),
    InvalidHeaderSize(u32),
    UnexpectedMyLba(u64),
    UnexpectedAlternateLba(u64),
    HeaderDoesNotMatchBackup,
    UnexpectedNonZeroValue,
    HeaderChecksumMismatch { computed: u32, expected: u32 },
    PartitionEntryArrayChecksumMismatch { computed: u32, expected: u32 },
    NoPartitions,
    InvalidMbrSignature(u16),
    BackupPartitionArrayDoesNotMatch,
}

impl fmt::Display for GptError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for GptError {}

#[derive(Debug)]
struct GptHeader {
    revision: u32,
    header_size: u32,
    header_crc32: u32,
    my_lba: u64,
    alternate_lba: u64,
    first_usable_lba: u64,
    last_usable_lba: u64,
    disk_guid: Uuid,
    partition_entry_lba: u64,
    number_of_partition_entries: u32,
    size_of_partition_entry: u32,
    partition_entry_array_crc32: u32,
}

const LOGICAL_BLOCK_SIZE: usize = 512;
const REQUIRED_SIGNATURE: u64 = 0x5452415020494645;
const THIS_REVISION: u32 = 0x10000;
const MIN_HEADER_SIZE: u32 = 92;

impl GptHeader {
    fn new_single_partition_raw(
        disk_size_in_lba: u64,
        disk_guid: Uuid,
        partition_entry_array_raw: &[u8],
    ) -> [u8; LOGICAL_BLOCK_SIZE as usize] {
        let my_lba: u64 = 1;
        let alternate_lba: u64 = disk_size_in_lba - 1;
        let first_usable_lba: u64 = 2 // mbr and primary gpt header
                + create::PARTITION_ARRAY_NUM_LBA;
        let last_usable_lba: u64 = disk_size_in_lba - 1 - create::PARTITION_ARRAY_NUM_LBA - 1;
        let partition_entry_lba: u64 = 2; // mbr and primary gpt header
        let mut raw = [0; LOGICAL_BLOCK_SIZE as usize];
        raw[0..8].copy_from_slice(&REQUIRED_SIGNATURE.to_le_bytes());
        raw[8..12].copy_from_slice(&THIS_REVISION.to_le_bytes());
        raw[12..16].copy_from_slice(&MIN_HEADER_SIZE.to_le_bytes());
        // gap for CRC at 16..20
        // 0 at 20..24
        raw[24..32].copy_from_slice(&my_lba.to_le_bytes());
        raw[32..40].copy_from_slice(&alternate_lba.to_le_bytes());
        raw[40..48].copy_from_slice(&first_usable_lba.to_le_bytes());
        raw[48..56].copy_from_slice(&last_usable_lba.to_le_bytes());
        raw[56..72].copy_from_slice(&guid::uuid_to_guid(disk_guid).to_le_bytes());
        raw[72..80].copy_from_slice(&partition_entry_lba.to_le_bytes());
        raw[80..84].copy_from_slice(&create::NUMBER_OF_PARTITION_ENTRIES.to_le_bytes());
        raw[84..88].copy_from_slice(&create::SIZE_OF_PARTITION_ENTRY.to_le_bytes());
        raw[88..92].copy_from_slice(&crc32::crc32(partition_entry_array_raw).to_le_bytes());
        let header_crc32 = crc32::crc32(&raw[0..(MIN_HEADER_SIZE as usize)]);
        raw[16..20].copy_from_slice(&header_crc32.to_le_bytes());
        raw
    }

    fn parse(raw: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let signature = u64::from_le_bytes(raw[0..8].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(GptError::InvalidSignature(signature).into());
        }
        let revision = u32::from_le_bytes(raw[8..12].try_into().unwrap());
        if revision != THIS_REVISION {
            return Err(GptError::IncorrectRevision(revision).into());
        }
        let header_size = u32::from_le_bytes(raw[12..16].try_into().unwrap());
        if header_size < MIN_HEADER_SIZE || header_size as usize > LOGICAL_BLOCK_SIZE {
            return Err(GptError::InvalidHeaderSize(header_size).into());
        }
        let header_crc32 = u32::from_le_bytes(raw[16..20].try_into().unwrap());
        let computed_crc32 = Self::crc32_from_logical_block(raw, header_size);
        if computed_crc32 != header_crc32 {
            return Err(GptError::HeaderChecksumMismatch {
                computed: computed_crc32,
                expected: header_crc32,
            }
            .into());
        }
        if u32::from_le_bytes(raw[20..24].try_into().unwrap()) != 0 {
            return Err(GptError::UnexpectedNonZeroValue.into());
        }
        let my_lba = u64::from_le_bytes(raw[24..32].try_into().unwrap());
        let alternate_lba = u64::from_le_bytes(raw[32..40].try_into().unwrap());
        let first_usable_lba = u64::from_le_bytes(raw[40..48].try_into().unwrap());
        let last_usable_lba = u64::from_le_bytes(raw[48..56].try_into().unwrap());
        let disk_guid = guid::guid_to_uuid(u128::from_le_bytes(raw[56..72].try_into().unwrap()));
        let partition_entry_lba = u64::from_le_bytes(raw[72..80].try_into().unwrap());
        let number_of_partition_entries = u32::from_le_bytes(raw[80..84].try_into().unwrap());
        let size_of_partition_entry = u32::from_le_bytes(raw[84..88].try_into().unwrap());
        let partition_entry_array_crc32 = u32::from_le_bytes(raw[88..92].try_into().unwrap());
        for &b in &raw[92..] {
            if b != 0 {
                return Err(GptError::UnexpectedNonZeroValue.into());
            }
        }
        Ok(Self {
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
        copy[0..header_size as usize].copy_from_slice(&logical_block[0..(header_size as usize)]);
        // zero-out the crc field of the copy
        copy[16..20].copy_from_slice(&0u32.to_le_bytes());
        crc32::crc32(&copy[0..(header_size as usize)])
    }

    fn partition_entry_array_byte_range(&self) -> Range<u64> {
        let partition_entry_array_start_index =
            self.partition_entry_lba * LOGICAL_BLOCK_SIZE as u64;
        let partition_entry_array_size =
            self.size_of_partition_entry * self.number_of_partition_entries;
        partition_entry_array_start_index
            ..(partition_entry_array_start_index + partition_entry_array_size as u64)
    }

    fn compare_header_and_backup_header(header: &Self, backup: &Self) -> Result<(), Error> {
        if header.my_lba == backup.alternate_lba
            || header.alternate_lba == backup.my_lba
            || header.disk_guid == backup.disk_guid
        {
            Ok(())
        } else {
            Err(GptError::HeaderDoesNotMatchBackup.into())
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
struct PartitionEntry {
    partition_type_guid: Uuid,
    unique_partition_guid: Uuid,
    starting_lba: u64,
    ending_lba: u64,
    attributes: u64,
    partition_name: String,
}

const PARITION_TYPE_GUID_EFI_SYSTEM_PARTITION_STR: &str = "C12A7328-F81F-11D2-BA4B-00A0C93EC93B";

mod gpt_partition_attributes {
    pub const REQUIRED_PARTITION: u8 = 0;
}

impl PartitionEntry {
    fn new_first_partition_with_size_in_lba_raw(
        partition_size_in_lba: u64,
        unique_partition_guid: Uuid,
        partition_name: String,
    ) -> [u8; create::SIZE_OF_PARTITION_ENTRY as usize] {
        let starting_lba: u64 = 2 // mbr and primary gpt header
                + create::PARTITION_ARRAY_NUM_LBA;
        let ending_lba: u64 = starting_lba + partition_size_in_lba - 1;
        let attributes: u64 = 1 << gpt_partition_attributes::REQUIRED_PARTITION;
        let max_num_chars = (128 - 56) / 2;
        let partition_name_encoded = partition_name
            .encode_utf16()
            .take(max_num_chars)
            .flat_map(|c| c.to_le_bytes().iter().cloned().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        debug_assert!(partition_name_encoded.len() <= (128 - 56));
        let mut raw = [0; create::SIZE_OF_PARTITION_ENTRY as usize];
        raw[0..16].copy_from_slice(
            &guid::uuid_to_guid(
                Uuid::parse_str(PARITION_TYPE_GUID_EFI_SYSTEM_PARTITION_STR).unwrap(),
            )
            .to_le_bytes(),
        );
        raw[16..32].copy_from_slice(&guid::uuid_to_guid(unique_partition_guid).to_le_bytes());
        raw[32..40].copy_from_slice(&starting_lba.to_le_bytes());
        raw[40..48].copy_from_slice(&ending_lba.to_le_bytes());
        raw[48..56].copy_from_slice(&attributes.to_le_bytes());
        raw[56..(56 + partition_name_encoded.len())].copy_from_slice(&partition_name_encoded);
        raw
    }

    fn new_array_single_partition_raw(
        partition_size_in_lba: u64,
        unique_partition_guid: Uuid,
        partition_name: String,
    ) -> [u8; create::SIZE_OF_PARTITION_ENTRY as usize * create::NUMBER_OF_PARTITION_ENTRIES as usize]
    {
        let mut raw = [0; create::SIZE_OF_PARTITION_ENTRY as usize
            * create::NUMBER_OF_PARTITION_ENTRIES as usize];
        raw[0..create::SIZE_OF_PARTITION_ENTRY as usize].copy_from_slice(
            &Self::new_first_partition_with_size_in_lba_raw(
                partition_size_in_lba,
                unique_partition_guid,
                partition_name,
            ),
        );
        raw
    }

    fn parse_array<'a>(
        raw: &'a [u8],
        header: &GptHeader,
    ) -> Result<impl 'a + Iterator<Item = Self>, Error> {
        let computed_crc32 = crc32::crc32(raw);
        if computed_crc32 != header.partition_entry_array_crc32 {
            return Err(GptError::PartitionEntryArrayChecksumMismatch {
                computed: computed_crc32,
                expected: header.partition_entry_array_crc32,
            }
            .into());
        }
        Ok(raw
            .chunks(header.size_of_partition_entry as usize)
            .map(Self::parse))
    }

    fn parse(bytes: &[u8]) -> Self {
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
            partition_type_guid: guid::guid_to_uuid(partition_type_guid),
            unique_partition_guid: guid::guid_to_uuid(unique_partition_guid),
            starting_lba,
            ending_lba,
            attributes,
            partition_name,
        }
    }

    fn partition_byte_range(&self) -> Range<u64> {
        (self.starting_lba as u64 * LOGICAL_BLOCK_SIZE as u64)
            ..((self.ending_lba as u64 + 1) * LOGICAL_BLOCK_SIZE as u64)
    }
}

mod mbr {
    pub const BOOT_CODE_SIZE: usize = 440;
    pub const PARTITION_RECORD_COUNT: usize = 4;
    pub const REQUIRED_SIGNATURE: u16 = 0xAA55;
    pub const UNIQUE_MBR_SIGNATURE_OFFSET: usize = BOOT_CODE_SIZE;
    pub const PARTITION_RECORD_OFFSET: usize = 446;
    pub const PARTITION_RECORD_SIZE: usize = 16;
    pub const SIGNATURE_OFFSET: usize = 510;
    pub const OS_TYPE_GPT_PROTECTIVE: u8 = 0xEE;
    pub const PARTITION_RECORD_MAX_ENDING_CHS: u32 = 0xFFFFFF;
    pub const PARTITION_RECORD_MAX_SIZE_IN_LBA: u32 = 0xFFFFFFFF;
}

struct BootCode {
    raw: [u8; mbr::BOOT_CODE_SIZE],
}

impl fmt::Debug for BootCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", &self.raw[..])
    }
}

impl Default for BootCode {
    fn default() -> Self {
        Self {
            raw: [0; mbr::BOOT_CODE_SIZE],
        }
    }
}

#[derive(Debug)]
struct Mbr {
    boot_code: BootCode,
    unique_mbr_disk_signature: u32,
    partition_record: [MbrPartitionRecord; mbr::PARTITION_RECORD_COUNT],
    signature: u16,
}

#[derive(Debug, Default, Clone, Copy)]
struct MbrPartitionRecord {
    boot_indicator: u8,
    starting_chs: u32,
    os_type: u8,
    ending_chs: u32,
    starting_lba: u32,
    size_in_lba: u32,
}

impl MbrPartitionRecord {
    fn new_protective_with_disk_size_in_lba(disk_size_in_lba: u64) -> Self {
        Self {
            boot_indicator: 0,
            starting_chs: 512,
            starting_lba: 1,
            os_type: mbr::OS_TYPE_GPT_PROTECTIVE,
            size_in_lba: (disk_size_in_lba - 1).min(mbr::PARTITION_RECORD_MAX_SIZE_IN_LBA as u64)
                as u32,
            ending_chs: (disk_size_in_lba * LOGICAL_BLOCK_SIZE as u64 - 1)
                .min(mbr::PARTITION_RECORD_MAX_ENDING_CHS as u64) as u32,
        }
    }
}

impl Mbr {
    fn new_protective_with_disk_size_in_lba(disk_size_in_lba: u64) -> Self {
        Self {
            boot_code: BootCode::default(),
            unique_mbr_disk_signature: 0,
            partition_record: [
                MbrPartitionRecord::new_protective_with_disk_size_in_lba(disk_size_in_lba),
                MbrPartitionRecord::default(),
                MbrPartitionRecord::default(),
                MbrPartitionRecord::default(),
            ],
            signature: mbr::REQUIRED_SIGNATURE,
        }
    }
    fn encode(&self) -> [u8; LOGICAL_BLOCK_SIZE] {
        let mut encoded = [0; LOGICAL_BLOCK_SIZE];
        (&mut encoded[0..mbr::BOOT_CODE_SIZE]).copy_from_slice(&self.boot_code.raw);
        (&mut encoded[mbr::UNIQUE_MBR_SIGNATURE_OFFSET..(mbr::UNIQUE_MBR_SIGNATURE_OFFSET + 4)])
            .copy_from_slice(&self.unique_mbr_disk_signature.to_le_bytes());
        for (i, partition_record) in self.partition_record.iter().enumerate() {
            let base_index = mbr::PARTITION_RECORD_OFFSET + (mbr::PARTITION_RECORD_SIZE * i);
            let partition_record_encoded =
                &mut encoded[base_index..(base_index + mbr::PARTITION_RECORD_SIZE)];
            partition_record_encoded[0] = partition_record.boot_indicator;
            (&mut partition_record_encoded[1..4])
                .copy_from_slice(&partition_record.starting_chs.to_le_bytes()[0..3]);
            partition_record_encoded[4] = partition_record.os_type;
            (&mut partition_record_encoded[5..8])
                .copy_from_slice(&partition_record.ending_chs.to_le_bytes()[0..3]);
            (&mut partition_record_encoded[8..12])
                .copy_from_slice(&partition_record.starting_lba.to_le_bytes());
            (&mut partition_record_encoded[12..16])
                .copy_from_slice(&partition_record.size_in_lba.to_le_bytes());
        }
        (&mut encoded[mbr::SIGNATURE_OFFSET..mbr::SIGNATURE_OFFSET + 2])
            .copy_from_slice(&self.signature.to_le_bytes()[0..2]);
        encoded
    }
    fn parse(raw: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let boot_code_raw = {
            let mut boot_code = [0; mbr::BOOT_CODE_SIZE];
            boot_code.copy_from_slice(&raw[0..mbr::BOOT_CODE_SIZE]);
            boot_code
        };
        let unique_mbr_disk_signature = u32::from_le_bytes(
            raw[mbr::UNIQUE_MBR_SIGNATURE_OFFSET..(mbr::UNIQUE_MBR_SIGNATURE_OFFSET + 4)]
                .try_into()
                .unwrap(),
        );
        let partition_record = {
            let mut partition_record = [MbrPartitionRecord::default(); mbr::PARTITION_RECORD_COUNT];
            for i in 0..mbr::PARTITION_RECORD_COUNT {
                let base = mbr::PARTITION_RECORD_OFFSET + i * mbr::PARTITION_RECORD_SIZE;
                let partition_record_bytes = &raw[base..(base + mbr::PARTITION_RECORD_SIZE)];
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
        let signature =
            raw[mbr::SIGNATURE_OFFSET] as u16 | ((raw[mbr::SIGNATURE_OFFSET + 1] as u16) << 8);
        if signature == mbr::REQUIRED_SIGNATURE {
            Ok(Self {
                boot_code: BootCode { raw: boot_code_raw },
                unique_mbr_disk_signature,
                partition_record,
                signature,
            })
        } else {
            Err(GptError::InvalidMbrSignature(signature).into())
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

#[derive(Debug)]
struct GptInfoBackupHeader {
    header: GptHeader,
    comparison: Result<(), Error>,
}

#[derive(Debug)]
pub struct GptInfo {
    mbr: Mbr,
    header: GptHeader,
    backup_header: Result<GptInfoBackupHeader, Error>,
    partition_entry_array: Vec<PartitionEntry>,
}

impl GptInfo {
    pub fn first_partition_byte_range(&self) -> Result<Range<u64>, Error> {
        let first_partition_entry = self
            .partition_entry_array
            .first()
            .ok_or(GptError::NoPartitions)?;
        Ok(first_partition_entry.partition_byte_range())
    }
}

pub fn gpt_info<H>(handle: &mut H) -> Result<GptInfo, Error>
where
    H: io::Seek + io::Read,
{
    let mut buf = vec![0; LOGICAL_BLOCK_SIZE];
    // read the mbr
    handle_read(
        handle,
        0 * LOGICAL_BLOCK_SIZE as u64,
        LOGICAL_BLOCK_SIZE,
        &mut buf,
    )?;
    let mbr = Mbr::parse(&buf)?;
    // read the gpt header
    handle_read(
        handle,
        1 * LOGICAL_BLOCK_SIZE as u64,
        LOGICAL_BLOCK_SIZE,
        &mut buf,
    )?;
    let header = GptHeader::parse(&buf)?;
    if header.my_lba != 1 {
        return Err(GptError::UnexpectedMyLba(header.my_lba).into());
    }
    let backup_header = {
        // read the backup gpt header
        handle_read(
            handle,
            header.alternate_lba * LOGICAL_BLOCK_SIZE as u64,
            LOGICAL_BLOCK_SIZE,
            &mut buf,
        )
        .and_then(|()| GptHeader::parse(&buf))
        .map(|backup_header| {
            let comparison = GptHeader::compare_header_and_backup_header(&header, &backup_header);
            GptInfoBackupHeader {
                header: backup_header,
                comparison,
            }
        })
    };
    // read the partition entry array
    let partition_entry_array_byte_range = header.partition_entry_array_byte_range();
    handle_read(
        handle,
        partition_entry_array_byte_range.start,
        (partition_entry_array_byte_range.end - partition_entry_array_byte_range.start) as usize,
        &mut buf,
    )?;
    let partition_entry_array = PartitionEntry::parse_array(&buf, &header)?.collect::<Vec<_>>();
    if let Ok(ref backup_header) = backup_header {
        // read the backup partition entry array
        let backup_partition_entry_array_byte_range =
            backup_header.header.partition_entry_array_byte_range();
        handle_read(
            handle,
            backup_partition_entry_array_byte_range.start,
            (backup_partition_entry_array_byte_range.end
                - backup_partition_entry_array_byte_range.start) as usize,
            &mut buf,
        )?;
        let backup_partition_entry_array =
            PartitionEntry::parse_array(&buf, &header)?.collect::<Vec<_>>();
        if backup_partition_entry_array != partition_entry_array {
            return Err(GptError::BackupPartitionArrayDoesNotMatch.into());
        }
    }
    Ok(GptInfo {
        mbr,
        header,
        backup_header,
        partition_entry_array,
    })
}

pub fn first_partition_byte_range<H>(handle: &mut H) -> Result<Range<u64>, Error>
where
    H: io::Seek + io::Read,
{
    gpt_info(handle)?.first_partition_byte_range()
}

const fn size_in_bytes_to_num_logical_blocks(size: u64) -> u64 {
    ((size - 1) / LOGICAL_BLOCK_SIZE as u64) + 1
}

mod create {
    pub const NUMBER_OF_PARTITION_ENTRIES: u32 = 4;
    pub const SIZE_OF_PARTITION_ENTRY: u32 = 128;
    pub const PARTITION_ARRAY_NUM_LBA: u64 = super::size_in_bytes_to_num_logical_blocks(
        NUMBER_OF_PARTITION_ENTRIES as u64 * SIZE_OF_PARTITION_ENTRY as u64,
    );
}

fn disk_size_in_lba(partition_size_in_lba: u64) -> u64 {
    // The disk must be large enough to contain the following:
    // - mbr (1 LB)
    // - primary gpt header (1 LB)
    // - primary partition entry array
    // - partition
    // - backup partition entry array
    // - backup gpt header (1 LB)
    //
    1 // mbr
        + 1 // primary gpt header
        + create::PARTITION_ARRAY_NUM_LBA // primary partition array
        + partition_size_in_lba
        + create::PARTITION_ARRAY_NUM_LBA // backup primary array
        + 1 // backup gpt header
}

pub fn write_header<H>(handle: &mut H, partition_size_bytes: u64) -> Result<(), Error>
where
    H: io::Write,
{
    let partition_size_in_lba = size_in_bytes_to_num_logical_blocks(partition_size_bytes);
    let disk_size_in_lba = disk_size_in_lba(partition_size_in_lba);
    let mbr_raw = Mbr::new_protective_with_disk_size_in_lba(disk_size_in_lba).encode();
    if let Err(ref e) = Mbr::parse(&mbr_raw) {
        eprintln!("Failed to parse generated MBR");
        die(e);
    }
    handle.write_all(&mbr_raw)?;
    let partition_entry_array = PartitionEntry::new_array_single_partition_raw(
        partition_size_in_lba,
        Uuid::new_v4(),
        "efi".to_string(),
    );
    let gpt_header = GptHeader::new_single_partition_raw(
        disk_size_in_lba,
        Uuid::new_v4(),
        &partition_entry_array,
    );
    let header_parsed = match GptHeader::parse(&gpt_header) {
        Ok(header) => header,
        Err(ref e) => {
            eprintln!("Failed to parse generated GPT header");
            die(e);
        }
    };
    if let Err(ref e) = PartitionEntry::parse_array(&partition_entry_array, &header_parsed) {
        eprintln!("Failed to parse generated partition entry array");
        die(e);
    }
    handle.write_all(&gpt_header)?;
    handle.write_all(&partition_entry_array)?;
    Ok(())
}

fn die(error: &Error) -> ! {
    eprintln!("{}", error);
    #[cfg(feature = "backtrace")]
    eprintln!("{}", error.backtrace());
    std::process::exit(1);
}
