use crate::rust::lib_gen::{Module, ModuleDef, ModuleName};
use indoc::indoc;

pub fn error_gen() -> Module {
    let code = indoc! { r#"
        use bytes::Bytes;

        pub enum Error<T> {
            Item(T),
            Reqwest(reqwest::Error),
            Serde(serde_json::Error),
            Unexpected {
                code: u16,
                data: Bytes,
            }
        }

        impl<T> Error<T> {
            pub fn unexpected(code: u16, data: Bytes) -> Error<T> {
                Error::Unexpected { code, data }
            }
        }

        impl<T> From<reqwest::Error> for Error<T> {
            fn from(value: reqwest::Error) -> Self {
                Error::Reqwest(value)
            }
        }

        impl<T> From<serde_json::Error> for Error<T> {
            fn from(value: serde_json::Error) -> Self {
                Error::Serde(value)
            }
        }
    "#};

    Module {
        def: ModuleDef {
            name: ModuleName::new("error"),
            exports: vec!["Error".to_string()],
        },
        code: code.to_string(),
    }
}
