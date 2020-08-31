use mini_fat::FatReader;
use mini_gpt::GptReader;

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
    let gpt = GptReader::new(&image_file_bytes).unwrap();
    let first_used_partition_index = gpt
        .metadata()
        .partition_entries()
        .iter()
        .enumerate()
        .find(|(_, entry)| entry.is_used())
        .map(|(index, _)| index);
    if let Some(first_used_partition_index) = first_used_partition_index {
        let partition_data = gpt.nth_partition_data(first_used_partition_index).unwrap();
        let fat = FatReader::new(partition_data.raw).unwrap();
        let root_directory = fat.root_directory();
        println!("{:#X?}", fat.bpb());
        println!("max valid cluster: {:X}", fat.maximum_valid_cluster());
        println!("{}", mini_hex_dump::Bytes(fat.fat_raw()));
        for e in root_directory.entries() {
            println!("{} {} {}", e.short_filename, e.first_cluster, e.file_size);
        }
        println!("fat entry: {:X}", fat.fat_entry_of_nth_cluster(3));
        for e in fat.directory_in_nth_cluster_tmp(3).entries() {
            println!("{} {} {}", e.short_filename, e.first_cluster, e.file_size);
        }
        for e in fat.directory_in_nth_cluster_tmp(4).entries() {
            println!("{} {} {}", e.short_filename, e.first_cluster, e.file_size);
        }
        //println!("{}", partition_data);
    }
}
