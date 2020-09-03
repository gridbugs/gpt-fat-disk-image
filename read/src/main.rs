struct Args {
    image_filename: String,
    cat_filename: String,
}

impl Args {
    fn parse() -> Self {
        use simon::Arg;
        (simon::args_map! {
            let {
                image_filename = simon::opt("i", "image", "path to disk image", "PATH").required();
                cat_filename = simon::opt("f", "file", "path within image of file to cat", "PATH").required();
            } in {
                Self { image_filename, cat_filename }
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
        cat_filename,
    } = Args::parse();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = mini_gpt::first_partition_byte_range(&mut image_file).unwrap();
    mini_fat::read_file(
        &mut image_file,
        first_partition_byte_range,
        cat_filename.as_str(),
    )
    .unwrap();
}
