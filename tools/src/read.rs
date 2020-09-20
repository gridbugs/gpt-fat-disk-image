use std::fs::File;
use std::io;

struct Args {
    image_filename: String,
    read_filename: String,
    output: Box<dyn io::Write>,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                image_filename = opt_req("PATH", 'i').name("image").desc("path to disk image");
                read_filename = opt_req("PATH", 'f').name("file").desc("path within image of file to read");
                output = opt_opt::<String, _>("PATH", 'o').name("output").desc("output file path (omit for stdout)");
            } in {
                Self {
                    image_filename,
                    read_filename,
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
        read_filename,
        mut output,
    } = Args::parse();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = mini_gpt::first_partition_byte_range(&mut image_file).unwrap();
    mini_fat::read_file(
        &mut image_file,
        first_partition_byte_range,
        read_filename.as_str(),
        &mut output,
    )
    .unwrap();
}
