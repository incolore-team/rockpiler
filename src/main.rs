use std::process::ExitCode;

use clap::Parser;
use log::*;
use rockc::{cli::Args, driver};

fn main() -> ExitCode {
    env_logger::init();
    let args = Args::parse();
    debug!("args: {:?}", args);
    driver::drive(args);
    return ExitCode::SUCCESS;
}

#[test]
fn test_all() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Trace)
        .init();

    use std::fs;

    let dir = "./tests/functional/";
    // let entries = fs::read_dir(dir).unwrap();
    let mut entries = fs::read_dir(dir)
        .unwrap()
        .map(|res| res.map(|e| e.path()).unwrap())
        .collect::<Vec<_>>();

    entries.sort();

    for path in entries {
        // let entry = entry.unwrap();
        // let path = entry.path();
        if !path.is_file() {
            continue;
        }
        match path.extension() {
            Some(extension) => {
                if extension != "sy" {
                    continue;
                }
            }
            None => continue,
        }
        info!("{}", path.to_str().unwrap());
        let file_stem = path.file_stem().unwrap().to_str().unwrap();
        let output_path = format!("{}{}.ll", dir, file_stem);
        let input_path = format!("{}{}.sy", dir, file_stem);
        let args = Args::parse_from(&["rockc", &input_path, "-o", &output_path]);
        driver::drive(args);
    }
}

#[test]
fn test_single() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Trace)
        .init();

    let dir = "./tests/functional/";
    let file_stem = "01_var_defn2";
    let output_path = format!("{}{}.ll", dir, file_stem);
    let input_path = format!("{}{}.sy", dir, file_stem);
    let args = Args::parse_from(&["rockc", &input_path, "-o", &output_path]);
    driver::drive(args);
}
