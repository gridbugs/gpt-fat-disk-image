use anyhow::Error;

pub fn die(error: &Error) -> ! {
    eprintln!("{}", error);
    #[cfg(feature = "backtrace")]
    eprintln!("{}", error.backtrace());
    std::process::exit(1);
}
