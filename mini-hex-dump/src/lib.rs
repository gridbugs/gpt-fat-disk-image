use std::fmt;

pub fn display_bytes(
    bytes: &[u8],
    bytes_per_row: usize,
    f: &mut fmt::Formatter,
) -> Result<(), fmt::Error> {
    let index_length = {
        let max_index: u32 = bytes.len().saturating_sub(1).min(u32::MAX as usize) as u32;
        let num_bits = 32 - max_index.leading_zeros();
        let num_nybles = if num_bits == 0 {
            0
        } else {
            ((num_bits - 1) / 4) + 1
        };
        num_nybles
    } as usize;
    for (row_index, row) in bytes.chunks(bytes_per_row).enumerate() {
        write!(
            f,
            "0x{: >width$X}: ",
            row_index * bytes_per_row,
            width = index_length,
        )?;
        for byte in row {
            write!(f, " {:02X}", byte)?;
        }
        writeln!(f, "")?;
    }
    Ok(())
}

pub struct Bytes<'a>(pub &'a [u8]);

impl<'a> fmt::Display for Bytes<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        display_bytes(self.0, 16, f)?;
        Ok(())
    }
}
