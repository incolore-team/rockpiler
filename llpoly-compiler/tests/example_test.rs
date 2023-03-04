use std::{ffi::OsString, fs, path::PathBuf};

use llpolyc::{cli_args, driver};

#[test]
fn test_examples() {
    // load source files from ../examples
    let examples_dir = "../examples";
    let files = glob(examples_dir, |filename| {
        filename
            .to_str()
            .map(|s| s.ends_with(".llpoly"))
            .unwrap_or(false)
    });
    let bin_name = "llpolyc".to_string();
    for file in files {
        println!("Compiling file {} ...", file.to_str().unwrap());
        let args = vec![bin_name.clone(), file.to_str().unwrap().to_string()];
        let cli_args = cli_args::CliArgs::from_args(&args);
        let driver = driver::Driver::new(&cli_args);
        let r = driver.run();
        match r {
            Ok(_) => println!("OK"),
            Err(e) => println!("Error: {}", e),
        }
    }
}

fn glob<F>(dir: &str, filter: F) -> Vec<PathBuf>
where
    F: Fn(&OsString) -> bool,
{
    let mut files = Vec::new();
    fs::read_dir(dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().unwrap().is_file())
        .filter(|entry| filter(&entry.file_name()))
        .for_each(|entry| files.push(entry.path()));
    files
}
