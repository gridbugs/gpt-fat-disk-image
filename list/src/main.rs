struct Args {
    image_filename: String,
    list_filename: String,
}

impl Args {
    fn parse() -> Self {
        use simon::Arg;
        (simon::args_map! {
            let {
                image_filename = simon::opt("i", "image", "path to disk image", "PATH").required();
                list_filename = simon::opt("f", "file", "path within image of file to list", "PATH")
                    .with_default_lazy(|| "/".to_string());
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
