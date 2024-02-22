// Copyright 2024 Golem Cloud
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use clap::Parser;
use golem_openapi_client_generator::gen;
use openapiv3::OpenAPI;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None, rename_all = "kebab-case")]
struct Command {
    #[arg(short, long, value_name = "spec", value_hint = clap::ValueHint::FilePath, num_args = 1.., required = true)]
    spec_yaml: Vec<PathBuf>,

    #[arg(short, long, value_name = "DIR", value_hint = clap::ValueHint::DirPath)]
    output_directory: PathBuf,

    #[arg(short = 'v', long, default_value = "0.0.0")]
    client_version: String,

    #[arg(short, long)]
    name: String,
}

fn main() {
    let command = Command::parse();

    let openapi_specs = command
        .spec_yaml
        .into_iter()
        .map(|spec| {
            let file = File::open(&spec).unwrap();
            let reader = BufReader::new(file);
            let openapi: OpenAPI = serde_yaml::from_reader(reader)
                .expect(format!("Could not deserialize input: {:?}", spec).as_str());
            openapi
        })
        .collect::<Vec<_>>();

    gen(
        openapi_specs,
        &command.output_directory,
        &command.name,
        &command.client_version,
    )
    .unwrap();
}
