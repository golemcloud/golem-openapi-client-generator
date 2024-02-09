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

use crate::printer::TreePrinter;
use crate::rust::model_gen::RefCache;
use crate::rust::printer::{rust_name, unit, RustContext};
use crate::{Error, Result};
use convert_case::{Case, Casing};
use openapiv3::{
    AdditionalProperties, IntegerFormat, ReferenceOr, Schema, SchemaKind, StringFormat, Type,
    VariantOrUnknownOrEmpty,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModelType {
    pub name: String,
}

pub type RustPrinter = TreePrinter<RustContext>;
pub type RustResult = Result<RustPrinter>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntFormat {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

impl ToString for IntFormat {
    fn to_string(&self) -> String {
        match self {
            IntFormat::U8 => "u8".to_string(),
            IntFormat::U16 => "u16".to_string(),
            IntFormat::U32 => "u32".to_string(),
            IntFormat::U64 => "u64".to_string(),
            IntFormat::I8 => "i8".to_string(),
            IntFormat::I16 => "i16".to_string(),
            IntFormat::I32 => "i32".to_string(),
            IntFormat::I64 => "i64".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataType {
    String,
    Uuid,
    DateTime,
    Boolean,
    Number,
    Int(IntFormat),
    Model(ModelType),
    Binary,
    Array(Box<DataType>),
    MapOf(Box<DataType>),
    Json,
}

pub fn escape_keywords(name: &str) -> String {
    if name == "type" {
        "r#type".to_string()
    } else {
        name.to_string()
    }
}

impl DataType {
    pub fn render_declaration(&self, top_param: bool) -> RustPrinter {
        fn to_ref(res_type: RustPrinter, prefer_ref: bool) -> RustPrinter {
            if prefer_ref {
                unit() + "&" + res_type
            } else {
                res_type
            }
        }

        match self {
            DataType::String => {
                if top_param {
                    unit() + "&str"
                } else {
                    unit() + "String"
                }
            }
            DataType::Uuid => {
                let res = rust_name("uuid", "Uuid");
                to_ref(res, top_param)
            }
            DataType::DateTime => {
                let res = rust_name("chrono", "DateTime") + "<" + rust_name("chrono", "Utc") + ">";
                to_ref(res, top_param)
            }
            DataType::Boolean => unit() + "bool",
            DataType::Number => unit() + "f64",
            DataType::Int(format) => unit() + format.to_string(),
            DataType::Binary => {
                if top_param {
                    unit() + "impl Into<" + rust_name("reqwest", "Body") + "> + Send"
                } else {
                    let res = rust_name("bytes", "Bytes");
                    to_ref(res, top_param)
                }
            }
            DataType::Model(ModelType { name }) => {
                let model_type = rust_name("crate::model", name);
                to_ref(model_type, top_param)
            }
            DataType::Array(item) => {
                if top_param {
                    unit() + "&[" + item.render_declaration(false) + "]"
                } else {
                    unit() + "Vec<" + item.render_declaration(false) + ">"
                }
            }
            DataType::MapOf(element) => {
                let res = rust_name("std::collections", "HashMap")
                    + "<String, "
                    + element.render_declaration(false)
                    + ">";
                to_ref(res, top_param)
            }
            DataType::Json => {
                let res = rust_name("serde_json::value", "Value");
                to_ref(res, top_param)
            }
        }
    }
}

pub fn ref_type_name(reference: &str, ref_cache: &mut RefCache) -> Result<DataType> {
    ref_cache.add(reference);

    let name = reference
        .strip_prefix("#/components/schemas/")
        .expect("Unexpected reference prefix.");

    Ok(DataType::Model(ModelType {
        name: name.to_case(Case::UpperCamel),
    }))
}

fn schema_type(schema: &Schema, ref_cache: &mut RefCache) -> Result<DataType> {
    match &schema.schema_kind {
        SchemaKind::Type(tpe) => match tpe {
            Type::String(string_type) => {
                if let VariantOrUnknownOrEmpty::Item(StringFormat::Binary) = &string_type.format {
                    Ok(DataType::Binary)
                } else if let VariantOrUnknownOrEmpty::Item(StringFormat::DateTime) =
                    &string_type.format
                {
                    Ok(DataType::DateTime)
                } else if let VariantOrUnknownOrEmpty::Unknown(format) = &string_type.format {
                    match format.as_str() {
                        "uuid" => Ok(DataType::Uuid),
                        _ => Ok(DataType::String),
                    }
                } else {
                    Ok(DataType::String)
                }
            }
            Type::Number(_) => Ok(DataType::Number),
            Type::Integer(int_type) => {
                if let VariantOrUnknownOrEmpty::Item(int_format) = &int_type.format {
                    match int_format {
                        IntegerFormat::Int32 => Ok(DataType::Int(IntFormat::I32)),
                        IntegerFormat::Int64 => Ok(DataType::Int(IntFormat::I64)),
                    }
                } else if let VariantOrUnknownOrEmpty::Unknown(format) = &int_type.format {
                    match format.as_str() {
                        "int8" => Ok(DataType::Int(IntFormat::I8)),
                        "int16" => Ok(DataType::Int(IntFormat::I16)),
                        "int32" => Ok(DataType::Int(IntFormat::I32)),
                        "int64" => Ok(DataType::Int(IntFormat::I64)),
                        "uint8" => Ok(DataType::Int(IntFormat::U8)),
                        "uint16" => Ok(DataType::Int(IntFormat::U16)),
                        "uint32" => Ok(DataType::Int(IntFormat::U32)),
                        "uint64" => Ok(DataType::Int(IntFormat::U64)),
                        _ => Ok(DataType::Int(IntFormat::I64)),
                    }
                } else {
                    Ok(DataType::Int(IntFormat::I64))
                }
            }
            Type::Boolean(_) => Ok(DataType::Boolean),
            Type::Object(obj) => {
                if !obj.properties.is_empty() {
                    Err(Error::unimplemented(
                        "Object parameter with properties is not supported.",
                    ))
                } else if let Some(ap) = &obj.additional_properties {
                    match ap {
                        AdditionalProperties::Any(_) => Err(Error::unimplemented(
                            "Object parameter with Any additional_properties is not supported.",
                        )),
                        AdditionalProperties::Schema(element_schema) => Ok(DataType::MapOf(
                            Box::new(ref_or_schema_type(element_schema, ref_cache)?),
                        )),
                    }
                } else {
                    Err(Error::unimplemented(
                        "Object parameter without additional_properties is not supported.",
                    ))
                }
            }
            Type::Array(arr) => {
                if let Some(items) = &arr.items {
                    let item_type = ref_or_box_schema_type(items, ref_cache)?;

                    Ok(DataType::Array(Box::new(item_type)))
                } else {
                    Err(Error::unimplemented(
                        "Array parameter without item is not supported.",
                    ))
                }
            }
        },
        SchemaKind::OneOf { .. } => Err(Error::unimplemented("OneOf parameter is not supported.")),
        SchemaKind::AllOf { .. } => Err(Error::unimplemented("AllOf parameter is not supported.")),
        SchemaKind::AnyOf { .. } => Err(Error::unimplemented("AnyOf parameter is not supported.")),
        SchemaKind::Not { .. } => Err(Error::unimplemented("Not parameter is not supported.")),
        SchemaKind::Any(_) => Ok(DataType::Json),
    }
}

pub fn ref_or_schema_type(
    ref_or_schema: &ReferenceOr<Schema>,
    ref_cache: &mut RefCache,
) -> Result<DataType> {
    match ref_or_schema {
        ReferenceOr::Reference { reference } => ref_type_name(reference, ref_cache),
        ReferenceOr::Item(schema) => schema_type(schema, ref_cache),
    }
}

pub fn ref_or_box_schema_type(
    ref_or_schema: &ReferenceOr<Box<Schema>>,
    ref_cache: &mut RefCache,
) -> Result<DataType> {
    match ref_or_schema {
        ReferenceOr::Reference { reference } => ref_type_name(reference, ref_cache),
        ReferenceOr::Item(schema) => schema_type(schema, ref_cache),
    }
}
