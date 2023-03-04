use std::{env, process::ExitCode};
use std::io::Write;

use llpolyc::{cli_args, driver, VERSION};
fn init_logger() {
    let log_fmt = |buf: &mut env_logger::fmt::Formatter, record: &log::Record| {
        writeln!(
            buf,
            "{}:{} [{}] - {}",
            record.file().unwrap_or("unknown"),
            record.line().unwrap_or(0),
            // chrono::Local::now().format("%Y-%m-%dT%H:%M:%S"),
            record.level(),
            record.args()
        )
    };
    env_logger::Builder::from_env(env_logger::Env::default().filter("LLPOLY_LOG"))
        .format(log_fmt)
        .init();
}
fn main() -> ExitCode {
    init_logger();
    let args = env::args().collect::<Vec<String>>();
    let cli_args = cli_args::CliArgs::from_args(&args);
    println!("{:?}\n", cli_args);

    if cli_args.help {
        println!(
            "Usage: {} [Options] <Input File>\n{}",
            cli_args.bin,
            cli_args::CLI_OPTIONS
        );
        return ExitCode::SUCCESS;
    }
    if cli_args.version {
        println!("Low Level Polygon Compiler - {}", VERSION);
        return ExitCode::SUCCESS;
    }
    if cli_args.input.is_empty() {
        println!("No input file specified");
        return ExitCode::FAILURE;
    }
    println!("Compiling {}", cli_args.input.join(", "));
    let driver = driver::Driver::new(&cli_args);
    match driver.run() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}