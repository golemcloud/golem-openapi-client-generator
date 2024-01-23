use crate::printer::NewLine;
use crate::toml::printer::{unit, StringContext};

pub fn gen(name: &str, version: &str) -> String {
    #[rustfmt::skip]
    let code = unit() +
        "[package]" + NewLine +
        r#"name = ""# + name + r#"""# + NewLine +
        r#"version = ""# + version + r#"""# + NewLine +
        r#"edition = "2021""# + NewLine +
        r#"license = "Apache-2.0""# + NewLine +
        r#"description = "Client for Golem Cloud's REST API""# + NewLine +
        NewLine +
        "[lib]" + NewLine +
        NewLine +
        "[dependencies]" + NewLine +
        r#"async-trait = "^0.1""# + NewLine +
        r#"bytes = "^1.5""# + NewLine +
        r#"chrono = { version = "^0.4", features = ["serde"] }"# + NewLine +
        r#"futures-core = "^0.3""# + NewLine +
        r#"http = "^1.0""# + NewLine +
        r#"reqwest = { version = "^0.11", features = ["gzip", "json", "multipart", "stream"] }"# + NewLine +
        r#"serde = { version = "^1.0", features = ["derive"] }"# + NewLine +
        r#"serde_json = "^1.0""# + NewLine +
        r#"tracing = "^0.1""# + NewLine +
        r#"uuid = { version = "^1.6", features = ["serde"] }"# + NewLine;

    StringContext::new().print_to_string(code)
}
