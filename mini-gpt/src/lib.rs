use std::io;
use std::ops::Range;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    InvalidSignature(u64),
    IncorrectRevision(u32),
    InvalidHeaderSize(u32),
    UnexpectedMyLba(u64),
    UnexpectedNonZeroValue,
    HeaderChecksumMismatch { computed: u32, expected: u32 },
    PartitionEntryArrayChecksumMismatch { computed: u32, expected: u32 },
    NoPartitions,
}

#[derive(Debug)]
struct GptHeader {
    revision: u32,
    header_size: u32,
    header_crc32: u32,
    my_lba: u64,
    alternate_lba: u64,
    first_usable_lba: u64,
    last_usable_lba: u64,
    disk_guid: u128,
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
    fn parse(raw: &[u8]) -> Result<Self, Error> {
        use std::convert::TryInto;
        let signature = u64::from_le_bytes(raw[0..8].try_into().unwrap());
        if signature != REQUIRED_SIGNATURE {
            return Err(Error::InvalidSignature(signature));
        }
        let revision = u32::from_le_bytes(raw[8..12].try_into().unwrap());
        if revision != THIS_REVISION {
            return Err(Error::IncorrectRevision(revision));
        }
        let header_size = u32::from_le_bytes(raw[12..16].try_into().unwrap());
        if header_size < MIN_HEADER_SIZE || header_size as usize > LOGICAL_BLOCK_SIZE {
            return Err(Error::InvalidHeaderSize(header_size));
        }
        let header_crc32 = u32::from_le_bytes(raw[16..20].try_into().unwrap());
        let computed_crc32 = Self::crc32_from_logical_block(raw, header_size);
        if computed_crc32 != header_crc32 {
            return Err(Error::HeaderChecksumMismatch {
                computed: computed_crc32,
                expected: header_crc32,
            });
        }
        if u32::from_le_bytes(raw[20..24].try_into().unwrap()) != 0 {
            return Err(Error::UnexpectedNonZeroValue);
        }
        let my_lba = u64::from_le_bytes(raw[24..32].try_into().unwrap());
        let alternate_lba = u64::from_le_bytes(raw[32..40].try_into().unwrap());
        let first_usable_lba = u64::from_le_bytes(raw[40..48].try_into().unwrap());
        let last_usable_lba = u64::from_le_bytes(raw[48..56].try_into().unwrap());
        let disk_guid = u128::from_le_bytes(raw[56..72].try_into().unwrap());
        let partition_entry_lba = u64::from_le_bytes(raw[72..80].try_into().unwrap());
        let number_of_partition_entries = u32::from_le_bytes(raw[80..84].try_into().unwrap());
        let size_of_partition_entry = u32::from_le_bytes(raw[84..88].try_into().unwrap());
        let partition_entry_array_crc32 = u32::from_le_bytes(raw[88..92].try_into().unwrap());
        for &b in &raw[92..] {
            if b != 0 {
                return Err(Error::UnexpectedNonZeroValue);
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
        copy.copy_from_slice(logical_block);
        // zero-out the crc field of the copy
        copy[16] = 0;
        copy[17] = 0;
        copy[18] = 0;
        copy[19] = 0;
        crc32(&copy[0..(header_size as usize)])
    }

    fn partition_entry_array_byte_range(&self) -> Range<usize> {
        let partition_entry_array_start_index =
            self.partition_entry_lba as usize * LOGICAL_BLOCK_SIZE;
        let partition_entry_array_size =
            (self.size_of_partition_entry * self.number_of_partition_entries) as usize;
        partition_entry_array_start_index
            ..(partition_entry_array_start_index + partition_entry_array_size)
    }
}

#[derive(Debug)]
struct PartitionEntry {
    partition_type_guid: u128,
    unique_partition_guid: u128,
    starting_lba: u64,
    ending_lba: u64,
    attributes: u64,
    partition_name: String,
}

impl PartitionEntry {
    fn parse_array<'a>(
        raw: &'a [u8],
        header: &GptHeader,
    ) -> Result<impl 'a + Iterator<Item = Self>, Error> {
        let computed_crc32 = crc32(raw);
        if computed_crc32 != header.partition_entry_array_crc32 {
            return Err(Error::PartitionEntryArrayChecksumMismatch {
                computed: computed_crc32,
                expected: header.partition_entry_array_crc32,
            });
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
            partition_type_guid,
            unique_partition_guid,
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

const CRC32_LUT: &[u32] = &[
    0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA, 0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3,
    0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988, 0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
    0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE, 0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
    0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC, 0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
    0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172, 0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,
    0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940, 0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
    0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116, 0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,
    0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924, 0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,
    0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A, 0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
    0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818, 0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
    0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E, 0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,
    0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C, 0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
    0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2, 0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,
    0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0, 0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
    0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086, 0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
    0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4, 0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,
    0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A, 0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,
    0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8, 0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
    0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE, 0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,
    0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC, 0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
    0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252, 0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
    0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60, 0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
    0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236, 0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,
    0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04, 0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
    0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A, 0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,
    0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38, 0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
    0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E, 0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
    0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C, 0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
    0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2, 0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,
    0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0, 0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
    0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6, 0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,
    0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94, 0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D,
];

fn crc32(data: &[u8]) -> u32 {
    let mut crc32 = 0xFFFFFFFF;
    for &byte in data {
        let index = ((crc32 ^ (byte as u32)) & 0xFF) as usize;
        crc32 = (crc32 >> 8) ^ CRC32_LUT[index];
    }
    crc32 ^ 0xFFFFFFFF
}

#[cfg(test)]
mod test {
    use super::crc32;

    #[test]
    fn spot_check() {
        assert_eq!(crc32(&[0x68, 0x65, 0x6C, 0x6C, 0x6F]), 0x3610A686);
    }
}

pub fn first_partition_byte_range<H>(handle: &mut H) -> Result<Range<u64>, Error>
where
    H: io::Seek + io::Read,
{
    let mut buf = vec![0; LOGICAL_BLOCK_SIZE];
    // skip over the mbr
    handle
        .seek(io::SeekFrom::Start(1 * LOGICAL_BLOCK_SIZE as u64))
        .map_err(Error::Io)?;
    // read the gpt header
    handle
        .read_exact(&mut buf[0..LOGICAL_BLOCK_SIZE])
        .map_err(Error::Io)?;
    let header = GptHeader::parse(&buf)?;
    if header.my_lba != 1 {
        return Err(Error::UnexpectedMyLba(header.my_lba));
    }
    let partition_entry_array_byte_range = header.partition_entry_array_byte_range();
    // resize buffer to fit partition entry array
    buf.resize(partition_entry_array_byte_range.len(), 0);
    // seek to start of partition entry array
    handle
        .seek(io::SeekFrom::Start(
            partition_entry_array_byte_range.start as u64,
        ))
        .map_err(Error::Io)?;
    // read partition entry array
    handle
        .read_exact(&mut buf[0..partition_entry_array_byte_range.len()])
        .map_err(Error::Io)?;
    let first_partition_entry = PartitionEntry::parse_array(&buf, &header)?
        .next()
        .ok_or(Error::NoPartitions)?;
    Ok(first_partition_entry.partition_byte_range())
}
