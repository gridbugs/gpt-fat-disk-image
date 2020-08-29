use mini_gpt::GptDisk;

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
    use std::io::Read;
    let Args { image_filename } = Args::parse();
    let image_file_bytes = {
        let mut image_file = File::open(image_filename).expect("unable to open file");
        let mut buf = Vec::new();
        image_file
            .read_to_end(&mut buf)
            .expect("unable to read file");
        buf
    };
    let disk = GptDisk::new(&image_file_bytes).unwrap();
    let first_used_partition_index = disk
        .metadata()
        .partition_entries()
        .iter()
        .enumerate()
        .find(|(_, entry)| entry.is_used())
        .map(|(index, _)| index);
    if let Some(first_used_partition_index) = first_used_partition_index {
        let partition_data = disk.nth_partition_data(first_used_partition_index).unwrap();
        println!("{}", partition_data);
    }
}
