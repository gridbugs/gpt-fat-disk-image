struct Args {
    image_filename: String,
    list_filename: String,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                image_filename = opt_req("PATH", 'i').name("image").desc("path to disk image");
                list_filename = opt_opt("PATH", 'f').name("file").desc("path within image of file to list")
                    .with_default_parse("/");
            } in {
                Self {
                    image_filename,
                    list_filename,
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
    } = Args::parse();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = mini_gpt::first_partition_byte_range(&mut image_file).unwrap();
    match mini_fat::list_file(&mut image_file, first_partition_byte_range, &list_filename).unwrap()
    {
        mini_fat::FatFile::Normal(_) => println!("{}", list_filename),
        mini_fat::FatFile::Directory(directory) => {
            for e in directory.entries() {
                println!("{}", e.name());
            }
        }
    }
}
