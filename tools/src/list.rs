use std::io;
use std::path;

mod error;

struct Args {
    image_filename: String,
    list_filename: String,
    partition_only: bool,
    recursive: bool,
}

impl Args {
    fn parse() -> Self {
        (meap::let_map! {
            let {
                image_filename = opt_req("PATH", 'i').name("image").desc("path to disk image");
                list_filename = opt_opt("PATH", 'f').name("file").desc("path within image of file to list")
                    .with_default_parse("/");
                partition_only = flag('p').name("partition-only").desc("expect image to be a partition instead of an entire disk");
                recursive = flag('r').name("recursive").desc("recursively list directories");
            } in {
                Self {
                    image_filename,
                    list_filename,
                    partition_only,
                    recursive,
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
        partition_only,
        recursive,
    } = Args::parse();
    env_logger::init();
    let mut image_file = File::open(image_filename).expect("unable to open file");
    let first_partition_byte_range = if partition_only {
        0..(image_file.metadata().unwrap().len())
    } else {
        error::or_die(mini_gpt::first_partition_byte_range(&mut image_file))
    };
    let mut reader = error::or_die(mini_fat::FatReader::new(
        &mut image_file,
        first_partition_byte_range,
    ));
    if recursive {
        error::or_die(recursive_list(&mut reader, &list_filename));
    } else {
        match error::or_die(reader.lookup(&list_filename)) {
            mini_fat::FatFile::Normal(_) => println!("{}", list_filename),
            mini_fat::FatFile::Directory(directory) => {
                for e in directory.entries() {
                    let name = e.name();
                    println!("{}", name);
                }
            }
        }
    }
}

fn format_path(path: &path::Path) -> String {
    use std::collections::VecDeque;
    use std::path::Component;
    let mut components = path.components().collect::<VecDeque<_>>();
    if let Some(first) = components.iter().next() {
        if first == &Component::RootDir {
            components.pop_front();
        }
    }
    let strings = components
        .iter()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    format!("/{}", strings.join("/"))
}

fn recursive_list<H: io::Seek + io::Read, P: AsRef<path::Path>>(
    reader: &mut mini_fat::FatReader<H>,
    path: P,
) -> Result<(), mini_fat::Error> {
    use std::collections::VecDeque;
    let mut queue = VecDeque::new();
    queue.push_back(path.as_ref().to_path_buf());
    while let Some(path) = queue.pop_front() {
        match reader.lookup(&path)? {
            mini_fat::FatFile::Normal(_) => println!("{}", path.to_string_lossy()),
            mini_fat::FatFile::Directory(directory) => {
                println!("{}:", format_path(&path));
                for e in directory.entries() {
                    let name = e.name();
                    println!("  {}", name);
                    if e.is_directory() {
                        if name != "." && name != ".." {
                            queue.push_back(format!("{}/{}", format_path(&path), name).into());
                        }
                    }
                }
                println!("");
            }
        }
    }
    Ok(())
}
