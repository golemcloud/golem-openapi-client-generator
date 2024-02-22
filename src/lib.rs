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

use crate::rust::lib_gen::{Module, ModuleDef, ModuleName};
use crate::rust::model_gen::RefCache;
use openapiv3::OpenAPI;
use std::path::Path;
use std::result;

mod merger;
pub(crate) mod printer;
mod rust;
mod toml;

#[derive(Debug, Clone)]
pub enum Error {
    Unexpected { message: String },
    Unimplemented { message: String },
}

impl Error {
    pub fn unexpected<S: Into<String>>(message: S) -> Error {
        Error::Unexpected {
            message: message.into(),
        }
    }
    pub fn unimplemented<S: Into<String>>(message: S) -> Error {
        Error::Unimplemented {
            message: message.into(),
        }
    }

    pub fn extend<S: Into<String>>(&self, s: S) -> Error {
        match self {
            Error::Unexpected { message } => Error::Unexpected {
                message: format!("{} {}", s.into(), message.clone()),
            },
            Error::Unimplemented { message } => Error::Unimplemented {
                message: format!("{} {}", s.into(), message.clone()),
            },
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

pub fn gen(openapi_specs: Vec<OpenAPI>, target: &Path, name: &str, version: &str) -> Result<()> {
    let open_api = merger::merge_all_openapi_specs(openapi_specs)?;

    let src = target.join("src");
    let api = src.join("api");
    let model = src.join("model");

    std::fs::create_dir_all(&api).unwrap();
    std::fs::create_dir_all(&model).unwrap();

    let context = rust::context_gen::context_gen(&open_api)?;
    std::fs::write(src.join(context.def.name.file_name()), &context.code).unwrap();

    let mut ref_cache = RefCache::new();

    let modules: Result<Vec<Module>> = open_api
        .tags
        .iter()
        .map(|tag| rust::client_gen::client_gen(&open_api, Some(tag.clone()), &mut ref_cache))
        .collect();

    let mut api_module_defs = Vec::new();

    for module in modules? {
        std::fs::write(api.join(module.def.name.file_name()), module.code).unwrap();
        api_module_defs.push(module.def.clone())
    }

    let mut known_refs = RefCache::new();
    let mut models = Vec::new();

    while !ref_cache.is_empty() {
        let mut next_ref_cache = RefCache::new();

        for ref_str in ref_cache.refs {
            if !known_refs.refs.contains(&ref_str) {
                let model_file =
                    rust::model_gen::model_gen(&ref_str, &open_api, &mut next_ref_cache)?;
                std::fs::write(model.join(model_file.def.name.file_name()), model_file.code)
                    .unwrap();
                models.push(model_file.def);
                known_refs.add(ref_str);
            }
        }

        let mut unknown_ref_cache = RefCache::new();

        for ref_str in next_ref_cache.refs {
            if !known_refs.refs.contains(&ref_str) {
                unknown_ref_cache.add(ref_str)
            }
        }

        ref_cache = unknown_ref_cache;
    }

    std::fs::write(
        src.join("api.rs"),
        rust::lib_gen::lib_gen("crate::api", &api_module_defs),
    )
    .unwrap();

    std::fs::write(
        src.join("model.rs"),
        rust::lib_gen::lib_gen("crate::model", &models),
    )
    .unwrap();

    let errors = rust::error_gen::error_gen();
    std::fs::write(src.join(errors.def.name.file_name()), &errors.code).unwrap();

    let module_defs = vec![
        context.def,
        ModuleDef::new(ModuleName::new_pub("api")),
        ModuleDef::new(ModuleName::new_pub("model")),
        errors.def,
    ];

    let lib = rust::lib_gen::lib_gen("crate", &module_defs);
    std::fs::write(src.join("lib.rs"), lib).unwrap();

    let cargo = toml::cargo::gen(name, version);
    std::fs::write(target.join("Cargo.toml"), cargo).unwrap();

    Ok(())
}
