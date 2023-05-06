use clap::Parser;

#[derive(Parser, Debug)] // requires `derive` feature
#[command(name = "rockc")]
#[command(bin_name = "rockc")]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// Source files to compile
    #[arg(value_name="FILES")]
    pub inputs: Vec<std::path::PathBuf>,

    /// Level of optimization
    #[arg(short = 'O', long, default_value_t = 0)]
    pub optimize_level: u8,

    #[arg(short = 'S', long, default_value_t = false)]
    pub assembly: bool,

    #[arg(short = 'o', long)]
    pub output: std::path::PathBuf
}