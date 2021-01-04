use mini_fat::FatInfo;
use mini_gpt::GptInfo;
use std::fmt;

mod error;

struct Args {
    image_filename: String,
    debug: bool,
    partition_only: bool,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                image_filename = opt_req("PATH", 'i').name("image").desc("path to disk image");
                debug = flag('d').name("debug").desc("print debugging info");
                partition_only = flag('p').name("partition-only").desc("expect image to be a partition instead of an entire disk");
            } in {
                Self {
                    image_filename,
                    debug,
                    partition_only,
                }
            }
        })
        .with_help_default()
        .parse_env_or_exit()
    }
}

#[derive(Debug)]
struct DisplayInfo {
    gpt_info: Option<GptInfo>,
    fat_info: FatInfo,
}

impl fmt::Display for DisplayInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use mini_fat::FatType;
        write!(f, "FAT Type: ")?;
        match self.fat_info.fat_type() {
            FatType::Fat12 => writeln!(f, "FAT12")?,
            FatType::Fat16 => writeln!(f, "FAT16")?,
            FatType::Fat32 => writeln!(f, "FAT32")?,
        }
        writeln!(f, "Num Clusters: {}", self.fat_info.num_clusters())?;
        Ok(())
    }
}

fn main() {
    use std::fs::File;
    let Args {
        image_filename,
        debug,
        partition_only,
    } = Args::parse();
    env_logger::init();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let (first_partition_byte_range, gpt_info) = if partition_only {
        (0..(image_file.metadata().unwrap().len()), None)
    } else {
        let gpt_info = error::or_die(mini_gpt::gpt_info(&mut image_file));
        (
            gpt_info.first_partition_byte_range().unwrap(),
            Some(gpt_info),
        )
    };
    let fat_info = error::or_die(mini_fat::fat_info(
        &mut image_file,
        first_partition_byte_range,
    ));
    let display_info = DisplayInfo { gpt_info, fat_info };
    if debug {
        println!("{:#?}", display_info);
    } else {
        println!("{}", display_info);
    }
}
