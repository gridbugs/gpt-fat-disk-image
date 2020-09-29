use mini_fat::FatInfo;
use std::fmt;

struct Args {
    image_filename: String,
    debug: bool,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                image_filename = opt_req("PATH", 'i').name("image").desc("path to disk image");
                debug = flag('d').name("debug").desc("print debugging info");
            } in {
                Self {
                    image_filename,
                    debug,
                }
            }
        })
        .with_help_default()
        .parse_env_or_exit()
    }
}

struct DisplayInfo {
    fat_info: FatInfo,
}

impl fmt::Display for DisplayInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use mini_fat::FatType;
        write!(f, "FAT Type: ")?;
        match self.fat_info.fat_type() {
            FatType::Fat12 => write!(f, "FAT12")?,
            FatType::Fat16 => write!(f, "FAT16")?,
            FatType::Fat32 => write!(f, "FAT32")?,
        }
        Ok(())
    }
}

fn main() {
    use std::fs::File;
    let Args {
        image_filename,
        debug,
    } = Args::parse();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = mini_gpt::first_partition_byte_range(&mut image_file).unwrap();
    let fat_info = mini_fat::fat_info(&mut image_file, first_partition_byte_range).unwrap();
    if debug {
        println!("{:#?}", fat_info);
    } else {
        println!("{}", DisplayInfo { fat_info });
    }
}
