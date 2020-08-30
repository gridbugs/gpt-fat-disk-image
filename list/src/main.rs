use mini_fat::Fat;
use mini_gpt::Gpt;

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
    let gpt = Gpt::new(&image_file_bytes).unwrap();
    let first_used_partition_index = gpt
        .metadata()
        .partition_entries()
        .iter()
        .enumerate()
        .find(|(_, entry)| entry.is_used())
        .map(|(index, _)| index);
    if let Some(first_used_partition_index) = first_used_partition_index {
        let partition_data = gpt.nth_partition_data(first_used_partition_index).unwrap();
        let fat = Fat::new(partition_data.raw).unwrap();
        let root_directory = fat.root_directory();
        println!("{:#X?}", fat.bpb());
        for e in root_directory.entries() {
            println!("{}", e.short_filename);
        }
        //println!("{}", partition_data);
    }
}
