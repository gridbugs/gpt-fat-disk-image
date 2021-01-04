mod error;

struct Args {
    image_filename: String,
    list_filename: String,
    partition_only: bool,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                image_filename = opt_req("PATH", 'i').name("image").desc("path to disk image");
                list_filename = opt_opt("PATH", 'f').name("file").desc("path within image of file to list")
                    .with_default_parse("/");
                partition_only = flag('p').name("partition-only").desc("expect image to be a partition instead of an entire disk");
            } in {
                Self {
                    image_filename,
                    list_filename,
                    partition_only,
                }
            }
        })
        .with_help_default()
        .parse_env_or_exit()
    }
}

fn main() {
    use std::fs::File;
    let Args {
        image_filename,
        list_filename,
        partition_only,
    } = Args::parse();
    env_logger::init();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = if partition_only {
        0..(image_file.metadata().unwrap().len())
    } else {
        error::or_die(mini_gpt::first_partition_byte_range(&mut image_file))
    };
    match error::or_die(mini_fat::list_file(
        &mut image_file,
        first_partition_byte_range,
        &list_filename,
    )) {
        mini_fat::FatFile::Normal(_) => println!("{}", list_filename),
        mini_fat::FatFile::Directory(directory) => {
            for e in directory.entries() {
                println!("{}", e.name());
            }
        }
    }
}
