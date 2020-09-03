use std::fs::File;
use std::io;

struct Args {
    image_filename: String,
    cat_filename: String,
    output: Box<dyn io::Write>,
}

impl Args {
    fn parse() -> Self {
        use simon::Arg;
        (simon::args_map! {
            let {
                image_filename = simon::opt("i", "image", "path to disk image", "PATH").required();
                cat_filename = simon::opt("f", "file", "path within image of file to cat", "PATH").required();
                output = simon::opt::<String>("o", "output", "output file path (omit for stdout)", "PATH");
            } in {
                Self {
                    image_filename,
                    cat_filename,
                    output: if let Some(path) = output {
                        Box::new(File::create(path).unwrap())
                    } else {
                        Box::new(io::stdout())
                    },
                }
            }
        })
        .with_help_default()
        .parse_env_or_exit()
    }
}

fn main() {
    let Args {
        image_filename,
        cat_filename,
        mut output,
    } = Args::parse();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = mini_gpt::first_partition_byte_range(&mut image_file).unwrap();
    mini_fat::read_file(
        &mut image_file,
        first_partition_byte_range,
        cat_filename.as_str(),
        &mut output,
    )
    .unwrap();
}
