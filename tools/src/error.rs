use anyhow::Error;

#[allow(dead_code)]
pub fn die(error: &Error) -> ! {
    eprintln!("{}", error);
    #[cfg(feature = "backtrace")]
    eprintln!("{}", error.backtrace());
    std::process::exit(1);
}

#[allow(dead_code)]
pub fn or_die<T>(result: Result<T, Error>) -> T {
    match result {
        Ok(t) => t,
        Err(ref e) => die(e),
    }
}
