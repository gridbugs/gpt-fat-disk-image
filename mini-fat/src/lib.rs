mod bpb;

pub use bpb::{Bpb, BpbError};

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Bpb(BpbError),
}

pub struct Fat<'a> {
    raw: &'a [u8],
    bpb: Bpb,
}

impl<'a> Fat<'a> {
    pub fn new(raw: &'a [u8]) -> Result<Self, Error> {
        let bpb = Bpb::new(raw).map_err(Error::Bpb)?;
        Ok(Self { raw, bpb })
    }

    pub fn bpb(&self) -> &Bpb {
        &self.bpb
    }
}
