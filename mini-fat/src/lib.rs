mod bpb;
mod directory;

pub use bpb::{Bpb, BpbError};
pub use directory::Directory;

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Bpb(BpbError),
}

pub struct FatReader<'a> {
    raw: &'a [u8],
    bpb: Bpb,
}

impl<'a> FatReader<'a> {
    pub fn new(raw: &'a [u8]) -> Result<Self, Error> {
        let bpb = Bpb::new(raw).map_err(Error::Bpb)?;
        Ok(Self { raw, bpb })
    }

    pub fn bpb(&self) -> &Bpb {
        &self.bpb
    }

    pub fn root_directory(&self) -> Directory {
        self.bpb.root_directory(self.raw)
    }

    pub fn fat_raw(&self) -> &[u8] {
        self.bpb.fat_raw(&self.raw)
    }

    pub fn maximum_valid_cluster(&self) -> u32 {
        self.bpb.maximum_valid_cluster()
    }

    pub fn fat_entry_of_nth_cluster(&self, n: u32) -> u32 {
        self.bpb.fat_entry_of_nth_cluster(self.fat_raw(), n)
    }

    pub fn directory_in_nth_cluster_tmp(&self, n: u32) -> Directory {
        self.bpb
            .directory_in_nth_cluster_tmp(self.bpb.clusters_raw(self.raw), n)
    }
}
