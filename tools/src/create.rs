use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::process;

#[derive(Debug)]
struct PathPair {
    in_local_filesystem: PathBuf,
    in_disk_image: PathBuf,
}

struct Args {
    path_pairs: Vec<PathPair>,
    output: Box<dyn io::Write>,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                local_filesystem_paths = opt_multi("PATH", 'l')
                    .name("local")
                    .desc("paths to local files to include in image (corresponds to -d)");
                disk_image_paths = opt_multi("PATH", 'd')
                    .name("disk")
                    .desc("paths in disk image where files will be stored (corresponds to -l)");
                output = opt_opt::<String, _>("PATH", 'o').name("output").desc("output file path (omit for stdout)");
            } in {{
                if local_filesystem_paths.len() != disk_image_paths.len() {
                    eprintln!("Error: -l and -d must be passed the same number of times.");
                    process::exit(1);
                }
                let path_pairs = local_filesystem_paths
                    .into_iter()
                    .zip(disk_image_paths.into_iter())
                    .map(|(in_local_filesystem, in_disk_image)| PathPair { in_local_filesystem, in_disk_image })
                    .collect();
                Self {
                    path_pairs,
                    output: if let Some(path) = output {
                        Box::new(File::create(path).unwrap())
                    } else {
                        Box::new(io::stdout())
                    },
                }
            }}
        })
        .with_help_default()
        .parse_env_or_exit()
    }
}

fn main() {
    let Args { path_pairs, output } = Args::parse();
    println!("{:?}", path_pairs);
}
