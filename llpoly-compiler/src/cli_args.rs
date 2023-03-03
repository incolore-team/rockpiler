pub const CLI_OPTIONS: &str = r#"
Options:
  -h, --help    Print this help message
  -v, --version Print the version of the compiler
  -o, --output  Specify the output file
"#;
#[derive(Debug, Default)]
pub struct CliArgs {
    pub bin: String,
    pub input: Vec<String>,
    pub output: Option<String>,
    pub help: bool,
    pub version: bool,
}

impl CliArgs {
    pub fn from_args(args: &Vec<String>) -> CliArgs {
        let mut cli_args = CliArgs::default();
        parse_args(args, &mut cli_args);
        cli_args
    }
}

fn parse_args(args: &Vec<String>, cli_args: &mut CliArgs) {
    cli_args.bin = args[0].clone();
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => cli_args.help = true,
            "-v" | "--version" => cli_args.version = true,
            "-o" | "--output" => {
                i += 1;
                cli_args.output = Some(args[i].clone());
            }
            _ => {
                cli_args.input.push(args[i].clone());
            }
        }
        i += 1;
    }
}
