struct Args {
    image_filename: String,
}

impl Args {
    fn simon() -> impl simon::Arg<Item = Self> {
        use simon::Arg;
        simon::args_map! {
            let {
                image_filename = simon::free().vec_singleton().required();
            } in {
                Self { image_filename }
            }
        }
    }
    fn parse() -> Self {
        use simon::Arg;
        Self::simon().with_help_default().parse_env_or_exit()
    }
}

fn main() {
    use std::fs::File;
    let Args { image_filename } = Args::parse();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = mini_gpt::first_partition_byte_range(&mut image_file).unwrap();
    let root_directory =
        mini_fat::root_directory(&mut image_file, first_partition_byte_range).unwrap();
    for e in root_directory.entries() {
        println!("{}", e.name());
    }
}
