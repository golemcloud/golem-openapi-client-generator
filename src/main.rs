use clap::Parser;
use golem_openapi_client_generator::gen;
use openapiv3::OpenAPI;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None, rename_all = "kebab-case")]
struct Command {
    #[arg(short, long, value_name = "spec", value_hint = clap::ValueHint::FilePath)]
    spec_yaml: PathBuf,

    #[arg(short, long, value_name = "DIR", value_hint = clap::ValueHint::DirPath)]
    output_directory: PathBuf,

    #[arg(short = 'v', long, default_value = "0.0.0")]
    client_version: String,

    #[arg(short, long)]
    name: String,
}

fn main() {
    let command = Command::parse();

    let file = File::open(command.spec_yaml).unwrap();

    let reader = BufReader::new(file);

    let openapi: OpenAPI = serde_yaml::from_reader(reader).expect("Could not deserialize input");

    gen(
        openapi,
        &command.output_directory,
        &command.name,
        &command.client_version,
    )
    .unwrap();
}
